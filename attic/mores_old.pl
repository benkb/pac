#!/usr/bin/env perl

# Copyright (c) 2019-present ben@kb8.ch
# Licensed under the MIT License

use strict;
use warnings;
use Data::Dumper 'Dumper';

my $USAGE = '<file>';
my $HELP = 'mores - writing s-expressions with withespace and other special characters';

my $blockbuster_closers = ')';
my $blockbuster_beginners = '(';

sub trace { print join(' ' , @_); print "\n";}

sub line_to_out {
}

sub set_lastline {
   my ($last_pair, $paren_stack, $out) = @_;

   my ($last_indent, $last_line_nr) = @$last_pair;

  if($last_line_nr){
    my ($terminal_parens) = join('', @$paren_stack );
    $out->[$last_line_nr]->[2] = $out->[$last_line_nr]->[2] . $terminal_parens;
    @$paren_stack = ();
  }
}

my %Trans = (
  def => 'define',
  defun => 'define'
);

sub pusher {
  my ($chars_ref, $line_ref) = @_;

   my $pushit; $pushit = sub {
     if(@$chars_ref){
       my $word = join('', @$chars_ref);
       push @$line_ref, (exists $Trans{$word}) ? $Trans{$word} : $word;
       #push @$line_ref, $_[0] if($_[0]);
       push @$line_ref, @_ ;
       undef @$chars_ref;
     }else{
       push @$line_ref, @_ ;
       #push @$line_ref, $_[0] if($_[0]);
     }
   };
}

sub transport_code {
   my ($instring_line, $code) = @_;
   my (@line, @chars) ;
   my $pushit = pusher \@chars, \@line;


   my ($prev, $line_closers, $lead_point, $lead_point_ws, $blockbuster_line, $in_backslash, $incomment_line)  = ('', 0);

   
   foreach (split //, $code){
      if($instring_line){
         if($_ eq '"'){
            $instring_line = undef;
            $pushit->('"');
         }else{
           push @chars, $_
         }
      }elsif($incomment_line){
      }else{
        if(/\s/){
          if($prev eq '$'){
            pop @chars;
            $pushit->('(');
            $line_closers++;
          }else{
            $pushit->(($lead_point_ws) ? () : ' ' ) ;
          }
        }else{
          undef $lead_point_ws;
          if(/\"/){
            $pushit->('"');
            $instring_line = 1;
          }elsif(/\\/){
            if($in_backslash){
               $pushit->(')');
               undef $in_backslash;
            }else{
               $in_backslash = 1;
               $pushit->('lambda (');
            }
          }elsif(/\=/){
            if($prev eq '='){
              $pushit->('=');
            }elsif(($prev eq ' ') or ($prev eq '(')){
             }else{
               $pushit->($_);
             }
          }elsif(/\{/){
            $pushit->('list ');
          }elsif(/\$/){
            $pushit->('');
          }elsif(/\(/){
            if(@chars){
              my $prefix = join('', @chars); undef @chars;
              push @line, ($prefix eq "'") ?  ($prefix, $_) : ($_, $prefix, ' ');
            }else{
              push @line, '(';
            }
          }elsif(/\=jsadkfjsdakfj/){
            #  }elsif(/\=/){
            die "Err: please only one blockbuster_line" if $blockbuster_line;
            die "Err: please blockbuster only at line start" if ((@line) or (@chars));
            @line=(); @chars=();
            #$pushit->('let (');
            $blockbuster_line = 'let (';
            $lead_point_ws = 1;
          }elsif(/\./){
            $lead_point = 1 unless $prev;
            $lead_point_ws = 1;
          }else{
              push @chars, $_
          }
        }
      }
      $prev = $_;
   }

   $pushit->();

   my $linestr = join('', @line);

   my $line = ($linestr eq ':')
      ? ')' x $line_closers
      :  $linestr .  ')' x $line_closers;


   #$pushit->(')' x $line_closers);


   #my $line = join('', @line);
   if($blockbuster_line){ $line = $blockbuster_line . '(' .  $line . ')'; }

   return ($lead_point, $line, $blockbuster_line, $instring_line);
}


sub set_prev_line_parens {
   my ($transp_code, $line_nr, $indent, $last_pair, $paren_stack, $out, $blockbuster_line)  = @_;

   my ($last_indent, $last_line_nr) = @$last_pair;

   if(defined $indent){
     if($indent == 0){
        $out->[$last_line_nr]->[2] = $out->[$last_line_nr]->[2] .  join('', @$paren_stack);
        @$paren_stack = ();
     }elsif($indent < $last_indent){ ### nested
         my ($term) = pop @$paren_stack;

         my $lower_line_nr = $last_line_nr - 1;;
         my @parens;
         for(my $i= $lower_line_nr; $i>0; $i--){
         #foreach (reverse @$out){
            my ($out_indent, undef, undef, $out_blockbuster) = @{$out->[$i] };

            if(defined $out_indent){
               if($indent < $out_indent){
                  if($out_indent >= $last_indent){
                     next
                  }else{
                     push @parens, pop @$paren_stack
                  }
               }elsif($indent > $out_indent){
                 last
               }else{ # ==
                 push @$paren_stack, ')' if $out_blockbuster;
                 push @parens, pop @$paren_stack;
                 last;
               }
             }else{
               next
             }
           }
         $out->[$last_line_nr]->[2] = $out->[$last_line_nr]->[2] . $term . join('', @parens);
      }elsif($indent > $last_indent){
        # default nothing todo
      }else { # ==
         my ($term) = pop @$paren_stack;
         $out->[$last_line_nr]->[2] = $out->[$last_line_nr]->[2] . $term;
      }
    }

   return 0
}

sub dolines {
   my $file = shift;

   my (@paren_stack, $blockbuster_active, $instring, ) = ();
   my @out = ([ undef, '', '']);  # indend , ws, trans-code 
   my ($line_nr, $indent, $multiline_indent) = (1,0, 0);
   my @last_pair = ();# ( $line_nr, $indent) ;

   foreach (@_){
      chomp;
      my ($ws,$code) = ( $_ =~ /^(\s*)(.*)/); $code =~ s/\s+$//;


      my ($lead_point, $transp_code, $blockbuster_line, $instring_line) = transport_code ($instring, $code);

			my ($indent, $start_paren, $end_paren) = (undef, '', ''); # default for empyt lines
      if($code){
			   # defaults:
         ($indent) =  scalar(split('', $ws) ) ; 
			   ($start_paren, $end_paren) = ($lead_point) ? ('','') : ('(',')');
            ($start_paren, $end_paren) = ('', '') if $blockbuster_active;

         if($code =~/^\#\s*(.*)/){
           $transp_code = "; $1"; 
            ($indent, $start_paren, $end_paren) = (undef,'','');
         }elsif($instring and $instring_line){ # " .... "
            ($indent, $start_paren, $end_paren) = (undef,'','');
         }elsif($instring_line){ # ".....
            $multiline_indent = $indent;
            $indent = undef;
        }elsif($instring){ # .... "
            ($indent, $start_paren) = ($multiline_indent,'');
            $blockbuster_active 
               = set_prev_line_parens($transp_code, $line_nr, $indent, \@last_pair, \@paren_stack, \@out, $blockbuster_line) if @paren_stack;
            @last_pair = ($multiline_indent, $line_nr);
            push @paren_stack, $end_paren ;
        }else{ # code
            if(/^\s*;/){ # comments
			         ($indent, $start_paren, $end_paren) = (undef, '', ''); # default for empyt lines
             }
            $blockbuster_active 
               = set_prev_line_parens($transp_code, $line_nr, $indent, \@last_pair, \@paren_stack, \@out, $blockbuster_line) if @paren_stack;
            @last_pair = ($indent, $line_nr);
            push @paren_stack, $end_paren ;
         }
      }

      push @out, [$indent, $ws, $start_paren . $transp_code, $blockbuster_line];
      
      #$blockbuster_active = 1 if $blockbuster_line;
      $line_nr++;
  }

   die "Err: string not closed" if $instring;

   set_lastline (\@last_pair, \@paren_stack, \@out); 
   return \@out;
}


sub main {
  my $file = $ARGV[0];

  die "usage: $USAGE" unless $file;
  die "Err: $file no valid file " unless -f $file;

  open (my $fh, '<', $file) || die "Err: no file";
  my @lines = <$fh>;
  close $fh;

  my ($out) = dolines $file, @lines;
  shift @$out;
  foreach (@$out){
    print $_->[1]  . $_->[2] . "\n";
  }

}


main () unless caller()
