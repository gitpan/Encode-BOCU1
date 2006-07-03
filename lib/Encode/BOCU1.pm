package Encode::BOCU1;
use strict;
use base qw(Encode::Encoding);

our $VERSION = '0.01';

__PACKAGE__->Define('bocu1');

use Encode::Alias;
define_alias( qr/^bocu.1$/i => '"bocu1"'); # BOCU-1, Bocu_1, bocu.1, ...
define_alias( qr/^bocu$/i => '"bocu1"');

#
# encode / decode
#
sub encode($$;$) {
    my ($obj, $str, $check) = @_;
    my $octet = utf8_to_bocu1($str);
    $_[1] = '' if $check; # $this is what in-place edit means
    return $octet;
}
sub decode ($$;$) {
    my ($obj, $octet, $check) = @_;
    my $str = bocu1_to_utf8($octet);
    $_[1] = '' if $check;
    return $str;
}

#
# Subroutines
#
# based on the sample C code available on http://www.unicode.org/notes/tn6/
# US Patent 6737994 "Binary-Ordered Compression For Unicode" by IBM
#
my @bocu1_trail_to_byte = (
#   0     1     2     3     4     5     6     7
    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x10, 0x11,
#   8     9     a     b     c     d     e     f
    0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
#   10    11    12    13
    0x1c, 0x1d, 0x1e, 0x1f );
my @bocu1_byte_to_trail = (
#   0     1     2     3     4     5     6     7
    -1,   0x00, 0x01, 0x02, 0x03, 0x04, 0x05, -1,
#   8     9     a     b     c     d     e     f
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
#   10    11    12    13    14    15    16    17
    0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
#   18    19    1a    1b    1c    1d    1e    1f
    0x0e, 0x0f, -1,   -1,   0x10, 0x11, 0x12, 0x13,
#   20
    -1 );

# Compute the next "previous" value for differencing from the current code point.
#
# @param $c : current code point, 0..0x10ffff
# @return "previous code point" state value
sub bocu1_prev {
    my $c = shift;

    if (0x3040 <= $c && $c <= 0x309f) {
        # Hiragana is not 128-aligned
        0x3070;
    } elsif (0x4e00 <= $c && $c <= 0x9fa5) {
        # CJI Unihan
        0x7711; # 0x4e00 - -10513
    } elsif (0xac00 <= $c && $c <= 0xd7a3) {
        # Korean Hangul
        0xc1d1; # (ac00 + d7a3)/2
    } else {
        # mostly small scripts
        ($c & ~0x7f) + 0x40;
    }
}

# Encode a difference -0x10ffff..0x10ffff in 1..4 bytes and return a BOCU-1 string
#
# The encoding favors small absolute differences with short encodings
# to compress runs of same-script characters.

# @param $diff : difference value -0x10ffff..0x10ffff
# @return $bocu1str : BOCU-1 string
sub pack_diff {
    my $diff = shift; ## -1114111 (=-0x10ffff) .. 1114111 (=0x10ffff)
    my ($lead,$count);

    if ($diff >= -64) {
        # mostly positive differences, and single-byte negative ones
        if ($diff <= 63) { # -64 .. 63
            # single byte
            return chr(0x90 + $diff); # 0x50 .. 0xcf
        } elsif ($diff <= 10512) { # 64 .. 10512
            # two bytes
            $diff -= 64; # 0 .. 10448 (= 43*243-1)
            $lead = 0xd0; # 0xd0 .. 0xfa
            $count = 1;
        } elsif ($diff <= 187659) { # 10513 .. 187659
            # three bytes
            $diff -= 10513; # 0 .. 177146 (= 3*243*243-1)
            $lead = 0xfb; # 0xfb .. 0xfd
            $count = 2;
        } else { # if ($diff <= 14536566) { # 187660 .. (14536566)
#            # four bytes
#            $diff -= 187660; # 0 .. 14348906 (1*243*243*243-1)
#            $lead = 0xfe; # 0xfe
#            $count = 3;
        }
    } else { # $diff < -64
        # two- and four-byte negative differences
        if ($diff >= -10513) { # -10513 .. -65
            # two bytes
            $diff -= -64; # -43*243 .. -1
            $lead = 0x50; # 0x25 .. 0x4f
            $count = 1;
        } elsif ($diff >= -187660) { # -187660 .. -10514
            # three bytes
            $diff -= -10513; # -3*243*243 .. -1
            $lead = 0x25; # 0x22 .. 0x24
            $count = 2;
        } else { # if ($diff >= -14536567) { # (-14536567) .. -187661
            # four bytes
            $diff -= -187660; # -1*243*243*243 .. -1
            $lead = 0x22; # 0x21
            $count = 3;
        }
    }

    # calculate trail bytes
    my $bocu1_str = '';
    for (my $i=$count; $i>0; $i--) {
        my $trail = $diff % 243;
        $diff = ($diff - $trail) / 243;
        
        my $byte = $trail >= 20 ? $trail + 13 : $bocu1_trail_to_byte[$trail];
        $bocu1_str = chr($byte) . $bocu1_str;
    }
    chr($lead + $diff) . $bocu1_str;
}

#
# BOCU-1 encoder function.
#
# @param \$prev : reference to the integer that holds
#        the "previous code point" state;
#        the initial value should be 0 which
#        encode_bocu1() will set to the actual BOCU-1 initial state value
# @param $c : the code point to encode
# @return $bocu1str : BOCU-1 string
#         or undef if an error occurs
#
# @see pack_diff()
#
sub encode_bocu1 {
    my ($ref_prev,$c) = @_;
    if (!defined($ref_prev) || $c < 0 || $c > 0x10ffff) {
        # ERROR : illegal argument
        return undef;
    }

    my $prev = $$ref_prev;
    if ($prev == 0) {
        # lenient handling of initial value 0
        $prev = $$ref_prev = 0x40;
    }

    if ($c <= 0x20) {
        #
        # ISO C0 control & space:
        # Encode directly for MIME compatibility,
        # and reset state except for space, to not disrupt compression.
        #
        if ($c != 0x20) {
            $$ref_prev = 0x40;
        }
        return chr($c);
    }

    #
    # all other Unicode code points $c==U+0021..U+10ffff
    # are encoded with the difference $c - $prev
    #
    # a new prev is computed from $c,
    # placed in the middle of a 0x80-block (for most small scripts) or
    # in the middle of the Unihan and Hangul blocks
    # to statistically minimize the following difference
    #
    $$ref_prev = &bocu1_prev($c);
    &pack_diff($c - $prev);
}

#
# Function for BOCU-1 decoder; handles multi-byte lead bytes.
#
# @param \%rx : reference to the decoder state structure { prev, count, diff }.
# @param $b : lead byte;
#          0x21 <= $b <  0x50
#       or 0xd0 <= $b <= 0xfe
# @return -1 (state change only)
#
# @see decode_bocu1()
#
sub decode_bocu1_lead_byte {
    my ($ref_rx, $b) = @_;
    my ($c,$count);

    if ($b >= 0x50) {
        # positive difference
        ## since d0 <= $b ...
        if ($b < 0xfb) { # d0 .. fa
            # two bytes
            $c = ($b - 0xd0) *243 + 63 + 1; # ( .. 42)*243 + 64  # .. 10270+r
            $count = 1;
        } elsif ($b < 0xfe) { # fb fc fd
            # three bytes
            $c = ($b - 0xfb) *243*243 + 10512 + 1; # (0..2)*243*243 + 10513  # 10513 .. 128611+r
            $count = 2;
        } else { # fe
            # four bytes
            $c = 187659 + 1; # 3 *243*243 * 10512 + 1  # 187660 .. 
            $count = 3;
        }
    } else {
        # negative difference
        if ($b >= 0x25) { # 25 .. 4f
            # two bytes
            $c = ($b - 0x50) * 243 - 64; # (-43 .. -1)*243-64 = -10513 .. -307
            $count = 1;
        } elsif ($b > 0x21) { # 22 23 24
            # three bytes
            $c = ($b - 0x25) *243*243 - 10513; # (-3 .. -1)*243*243 - 10513 = -187660 .. -69562
            $count = 2;
        } else {
            # four bytes
            $c = -243*243*243 - 187660; # -1*243*243*243 - 187660 = -14536567
            $count = 3;
        }
    }

    # set the state for decoding the trail byte(s)
    $$ref_rx{diff} = $c;
    $$ref_rx{count} = $count;

    -1;
}

#
# Function for BOCU-1 decoder; handles multi-byte trail bytes.
#
# @param \%rx : reference to the decoder state structure
# @param $b : trail byte
# @return result value, same as decodeBocu1
#
# @see decode_bocu1()
#
sub decode_bocu1_trail_byte {
    my ($ref_rx, $b) = @_;
    my ($t, $c, $count);

    if ($b <= 0x20) {
        # skip some C0 controls and make the trail byte range contiguous
        $t = $bocu1_byte_to_trail[$b];
        if ($t < 0) {
            # illegal trail byte value
            $$ref_rx{prev} = 0x40;
            $$ref_rx{count} = 0;
            return -99;
        }
    } else {
        $t = $b - 13; # BOCU1_TRAIL_BYTE_OFFSET;
    }

    # add trail byte into difference and decrement count
    $c = $$ref_rx{diff};
    $count = $$ref_rx{count};

    if ($count == 1) {
        # final trail byte, deliver a code point
        $c = $$ref_rx{prev} + $c + $t;
        if (0 <= $c && $c <= 0x10ffff) {
            # valid code point result
            $$ref_rx{prev} = &bocu1_prev($c);
            $$ref_rx{count} = 0;
            return $c;
        } else {
            # illegal code point result
            $$ref_rx{prev} = 0x40;
            $$ref_rx{count} = 0;
            return -99;
        }
    }

    # intermediate trail byte
    if ($count == 2) {
        $$ref_rx{diff} = $c + $t * 243;
    } else { # if ($count == 3) {
        $$ref_rx{diff} = $c + $t * 243 * 243;
    }
    $$ref_rx{count} = $count - 1;
    -1;
}

#
# BOCU-1 decoder function.
#
# @param \%rx : reference to the decoder state structure;
#        the initial values should be 0 which
#        decodeBocu1 will set to actual initial state values
# @param $b : an input byte
# @return
#      0..0x10ffff for a result code point
#      -1 if only the state changed without code point output
#     <-1 if an error occurs
#
sub decode_bocu1 {
    my ($ref_rx, $b) = @_;
    my ($prev, $c, $count);

    return -99 unless defined($ref_rx); ## ERROR: illegal argument

    $prev = $$ref_rx{prev};
    if ($prev == 0) {
        # lenient handling of initial 0 values
        $prev = $$ref_rx{prev} = 0x40;
        $count = $$ref_rx{count} = 0;
    } else {
        $count = $$ref_rx{count};
    }

    if ($count == 0) {
        # byte in lead position
        if ($b <= 0x20) {
            #
            # Direct-encoded C0 control code or space.
            # Reset prev for C0 control codes but not for space.
            #
            if ($b != 0x20) {
                $$ref_rx{prev} = 0x40;
            }
            return $b;
        }

        #
        # $b is a difference lead byte.
        #
        # Return a code point directly from a single-byte difference.
        #
        # For multi-byte difference lead bytes, set the decoder state
        # with the partial difference value from the lead byte and
        # with the number of trail bytes.
        #
        # For four-byte differences, the signedness also affects the
        # first trail byte, which has special handling farther below.
        #
        if ($b >= 0x50 && $b < 0xd0) { # 50 .. cf
            # single-byte difference
            $c = $prev + ($b - 0x90);
            $$ref_rx{prev} = &bocu1_prev($c);
            return $c;
        } elsif ($b == 0xff) { # BOCU1_RESET
            # only reset the state, no code point
            $$ref_rx{prev} = 0x40;
            return -1;
        } else {
            return decode_bocu1_lead_byte($ref_rx, $b);
        }
    } else {
        # trail byte in any position
        return decode_bocu1_trail_byte($ref_rx, $b);
    }
}

#
# Decode a BOCU-1 byte sequence to a UCS-4 codepoint stream.
#
# @param : $bocu1str : input BOCU-1 string
# @return : @codepoints : UCS-4 codepoint stream
#
sub bocu1_to_codepoints {
    my $bocu1str = shift;
    my @chars = unpack("C*", $bocu1str);

    my @codepoints = ();
    my %rx = ( prev => 0, count => 0, diff => 0 );

    for (my $i=0; $i<=$#chars; $i++) {
        my $c = &decode_bocu1(\%rx, $chars[$i]);
        if ($c < -1) {
            ## ERROR: "error: readString detects encoding error at string index %ld\n", i
            return -1;
        }
        if ($c >= 0) {
            push(@codepoints, $c)
        }
    }
    return @codepoints;
}

###
sub utf8_to_bocu1 {
    my $utf8str = shift;
    my $bocu1str = '';

    my @codepoints = unpack("U*", $utf8str);
    my $prev = 0;
    for (my $i=0; $i<=$#codepoints; $i++) {
        my $codepoint = $codepoints[$i];
        next if $codepoint == 0xfeff && $i == 0;

        $bocu1str .= &encode_bocu1(\$prev, $codepoint);
    }
    $bocu1str;
}

sub bocu1_to_utf8 {
    my $bocu1str = shift;

    my @codepoints = &bocu1_to_codepoints($bocu1str);
    my $utf8str = pack("U*", @codepoints);

    Encode::_utf8_on($utf8str);

    $utf8str;
}

1;
__END__

=head1 NAME

Encode::BOCU1 -- encodes / decodes BOCU-1 string, works as part of Encode.pm

=head1 SYNOPSIS

use Encode::BOCU1;

$string = 'Some UTF-8 text to convert'
Encode::from_to($string,'utf8','bocu1');
Encode::from_to($string,'bocu1','shiftjis');

=head1 DESCRIPTION

BOCU-1 is a MIME-compatible application of the Binary Ordered Compression for Unicode
[BOCU] base algorithm developed and patented by IBM.

Encode::BOCU1 enables to convert any encoding systems supported by Encode.pm
from/to BOCU-1 through UTF-8.

=head1 SEE ALSO

http://www.unicode.org/notes/tn6/
http://icu.sourceforge.net/docs/papers/binary_ordered_compression_for_unicode.html

=head1 COPYRIGHT AND LICENSE

This is pure-perl port of "BOCU-1 Sample C Code" written by Markus W. Scherer on 2002jan24,
available from http://www.unicode.org/notes/tn6/.

Ported by Naoya Tozuka E<lt>naoyat@naochan.comE<gt>

As with the original C code, this port is licensed under the X license (ICU version).
ICU License : http://dev.icu-project.org/cgi-bin/viewcvs.cgi/*checkout*/icu/license.html

BOCU "Binary-Ordered Compression For Unicode" is a patent-protected technology of IBM.
(US Patent 6737994)

=cut
