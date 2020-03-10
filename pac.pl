#!/usr/bin/env perl
#
use strict;
use warnings;

use Data::Dumper 'Dumper';

sub echo { print join(' ', grep { $_ if $_ } @_); print "\n"; }

my %Lisp_dialects = (
	clojure => {
		toplevel_defun => '%s [',
    post_doublepoint => 1,
    pre_doublepoint => 1,
    trans =>{
      'lambda_open' => 'fn [',
      'lambda_close' => ']',
      'lambda_short' => '](',
      'cliffhanger_open' => '[ ',
      'cliffhanger_close' => ']',
      'def' => 'def',
      'defun' => 'defn',
      'defun_close' => ']',
      '}' => '}',
      '{' => '{',
      '{{' => ')',
      '[[' => ')',
      '[' => ']',
      ']]' => ']',
      ']' => ')',
      'list' => '[',
      alist => '[ (values',
      alist_get => '(alist_get',
      ',' => ' ',
      'post_doublepoint_start' => '[',
      'post_doublepoint_end' => ']',
    },
	},
	scheme => {
		toplevel_defun => '(%s ',
    mod_form => undef,
    post_doublepoint => undef,
    pre_doublepoint => undef,
    trans =>{
      'match_list_open' => '(',
      'match_list_close' => ')',
      'lambda_open' => 'lambda (',
      'lambda_close' => ')',
      'lambda_short' => ')(',
      'cliffhanger_open' => '(',
      'cliffhanger_close' => ')',
      'def' => 'define',
      'defun' => 'define',
      'defun_close' => ')',
      '}' => '))',
      '{' => '(list (list',
      '{{' => ')',
      '[' => '(list',
      ']' => ')',
      ']]' => ')',
      ')' => ')',
      alist => '[ (values',
      alist_get => '(alist_get',
      ',' => ')(list',
      'list' => '(list',
      'post_doublepoint_start' => '((',
      'post_doublepoint_end' => '))',
      'mid_doublepoint' => ')(',
    },
	},
	ocaml => {
    parenswitch => 1,
    trans =>{
      'lambda_open' => 'lambda (',
      'lambda_close' => ')',
      'def' => 'define',
      'defun' => 'define',
      '}' => ')]',
      '{' => ')]',
      '{{' => ')',
      '[' => '(list',
      ']' => ')',
      alist => '[ (values',
      alist_get => '(alist_get',
      ',' => ')(values',
    },
	}
);

sub lines_loop {
   my ($file_lines, $dialect, $trans, $pp) = @_;


   my ($instring, $prev, @word, @line, @lines);

   my ($wordcount, @paren_stack) = (0);
   my $word2line; 
   my ($linestarted, $pre_doublepoint, $post_doublepoint, $mid_doublepoint, $match_pattern);
   my @lineparens;
	my $toplevel_defun;
  my ($lambda_close, $lambda_open, $lambda_singleline_confirm, $lambda_singleline);

   my $whitecheck; $whitecheck = sub {
      my ($eol ) = @_;

      return unless $prev; # for comments adn preprocs


         if($prev eq '='){
            if(join('', @word) eq '='){
							@word = ();
            }elsif(join('', @word) eq '=='){
							@word = ();
							push @line, '=';
						}
         }elsif($prev eq '>'){
            if(join('', @word) eq '->'){
              undef $match_pattern;
              @word = ();
            } elsif(join('', @word) eq '=>'){
               my $one = pop @line;
               if($one eq ' '){
                  @word = ();                        
                  my $two = pop @line;
                  # TODO: check two
                  push @line, $trans->{defun} , ' ',  $two;
                  #push @line,  $two;
									$toplevel_defun = 1;
               }else{
                 die 'TODO: checking ='
               }
            }
         }elsif($prev eq '\\'){
               if($lambda_close){
                 die "Err: lambda already closed"
               }else{
                  if($lambda_open){   # inside \x\
                     $lambda_close = 1;
                     undef $lambda_open;
                     $lambda_singleline_confirm = 1 if $lambda_singleline;
                     $word2line->($trans->{lambda_close}, '(');
                     push @lineparens, ')';
                 }else{  # not yet inside \x\
                 }
               }
         }elsif($prev eq ':'){
            if(join('', @word) eq ':'){
               if($eol){ 
                  @word = ();
                  $post_doublepoint = $dialect->{post_doublepoint};
                }else{
                  if ($linestarted){
                     @word = ();
                    if($mid_doublepoint){
                     $word2line->($trans->{mid_doublepoint});
                    }else{
                      $mid_doublepoint = 1;
                     $word2line->($trans->{post_doublepoint_start});
                     push @lineparens, $trans->{post_doublepoint_end};
                    }
                   }else{
                     @word = ();
                     #$word2line->($trans->{pre_doublepoint_start});
                     $pre_doublepoint = $dialect->{pre_doublepoint};
                     $linestarted = 1;
                   }
                }
              }elsif(join('', @word) eq '=:'){
               my $one = pop @line;
                  if($one eq ' '){
                     @word = ();                        
                     my $two = pop @line;
                     # TODO: check two
                     push @line, $trans->{def} , ' ',  $two;
                  }else{
                     die 'TODO: checking ='
                  }
            }else{ 
               die 'todo'; 
            }
           # if(join('', @word) eq '::'){
           #    push @line, 'let';
           #    @word = ();
           #    $post_doublepoint = $dialect->{post_doublepoint};
           # }
         }
         unless($eol){
           #echo 'jsdf' . Dumper \@line;
           $word2line->(' ')
         }
   };

   $word2line = sub {
      if(@word){
         $wordcount++ unless @paren_stack;

         my ($w) = join('', @word);
         if($dialect->{mod_form}){
            if($w =~ /\w+\.\w+/){
               my @ws = split (/\./, $w);
               push @line, ': ' . join(' ', @ws);
            }else{
               push @line, $w; 
            }
         }else{
            push @line, $w;
         }
         @word = ();
      }
      if(@_){
         push @line, @_ ;
      }
   };

   my $out_preproc;
   my $setup_preproc = sub {
     my ($codeline) = @_;
     substr($codeline, 0, 1) = "";
     my ($cmd, $arg) = split(/\s+/, $codeline);
     if ($cmd eq 'case'){
       if($arg eq $pp){
         $out_preproc = undef; 
       }else{
         $out_preproc = 1; 
       }
     }elsif($cmd eq 'esac'){
         $out_preproc = undef; 
     }else{
       die 'todo ' . $cmd
     }
   };

   my $i = 0;
   foreach (@$file_lines) {
      $i++;chomp;
      my ($fileline, $indent) = ($_, undef);
      @lineparens = ();

      $wordcount = 0 unless (@paren_stack);

      ($linestarted, $post_doublepoint, $pre_doublepoint, $mid_doublepoint, $toplevel_defun, $match_pattern) = (undef,undef, undef, undef, undef, undef);

      my $codeline;
      if($instring){
        $codeline = $fileline;
      }else{
         my ($ws,$code) = ( $_ =~ /^(\s*)(.*)/); $code =~ s/\s+$//;
         if($code =~ /=\w.+/){
            $setup_preproc->($code);
            push @lines, [ undef, ['; ' . $code], undef, undef, undef, undef, undef] ; 
            @word = (); @line = ();
           next;
         }elsif($out_preproc){
            push @lines, [ undef, ['; ' . $code], undef, undef, undef, undef, undef] ; 
            next;
         }else{
            ($codeline) = $code;
            $indent = split('', $ws);
          }
      }

      my ($lambda_short,$lambda_word, $comment ) ;
      ($lambda_close, $lambda_open, $lambda_singleline_confirm, $lambda_singleline,) = (undef, undef, undef, undef);
      foreach(split('', $codeline)){
         if($instring){
            if(/\"/){
               $word2line->($_);
               undef $instring;
            }else{
               push @word, $_;
            }
         }else{
            if(/\s/){
               if($linestarted){ 
                 $whitecheck->() unless ($prev eq ' ' ); 
               }else{
                 if ($prev eq ':' ){ 
                  $whitecheck->() ;
                  }
               }
             }elsif(/\;/){
               if($linestarted){
                 die "Err: only full line comments atm"
               }else{
                 $comment = $fileline;
                 @word = (); @line = ();
                 last;
               }
             }elsif(/\:/){
               push @word, $_;
               $lambda_close = undef;
               #$prev = $_ ; # preventing it from going into $linestaredsf
             }elsif(/\|/){
               $match_pattern = 1;
             }elsif(/\\/){
               if($lambda_close){
                 die "Err: lambda already closed"
               }else{
                  if($lambda_open){   # inside \x\
                    # pass it on and catch it on whitecheck();
                 }else{  # not yet inside \x\
                   $lambda_open = 1;
                   $lambda_short = undef;
                   my $shortform_arg = join('', @word);   # this is the x\(...) form
                   #die "Err: invalid form x\(jsd)" if @word;
                   @word = ();
                   if($linestarted){
                      if(@paren_stack){ # for example inside { ...
                         push @line , ('(', $trans->{lambda_open} );
                         #push @line , ((($shortform_arg) ? () :'('),'(', $trans->{lambda_open} );
                       }else{
                         push @line , ((($shortform_arg) ? ('(') :'('), $trans->{lambda_open} );
                       }
                      $lambda_singleline = undef;
                    }else{  # start of the line
                     push @line , ($trans->{lambda_open} );
                     $lambda_singleline = 1;
                    }
                   if($shortform_arg){ # the \x(...) form
                     #die xxxx => $trans->{lambda_close};
                     #die 'jsdf' . Dumper $shortform_arg;
                     $lambda_short = 1;
                     push @line, ($shortform_arg, $trans->{lambda_close}, '(');
                   }
                 }
               }
            }else{
               $lambda_close = undef;
              $lambda_singleline_confirm = undef;
               if(/\"/){
                  $word2line->();
                  push @word, $_;
                  $instring = 1;
               }elsif(/\./){
                 if(@word){
                  push @word, $_;
                }else{
                  push @line, ':'
                }
               }elsif(/\$/){
                  $word2line->('(');
                  push @lineparens, ')';
               }elsif(/\(/){
                  my ($prefix) =  join('', @word);  
                  @word = ();
                  $wordcount++ unless(@paren_stack);
                  if($prefix){
                     if($lambda_open){
                       undef $lambda_open ;
                        $lambda_short = 1;
                        push @paren_stack, '))';
                        push @line , $prefix , $trans->{lambda_short}  ;
                     }else{
                        #$word2line->($prefix, ' ');
                        push @line, '(',  $prefix, ' ';
                        push @paren_stack, ')';
                      }
                  }else{
                    if($toplevel_defun){
											my $tpl = $dialect->{toplevel_defun};
                      my $ws = pop @line;
                      my $fun_name = pop @line;
											my $txt = sprintf $tpl, $fun_name;
                      push @line, $txt  ;
                    }else{
                     $word2line->($_);
                    }
                     push @paren_stack, ')';
                  }
               }elsif(/\)/){
                  my $p = $paren_stack[-1];
                  if($toplevel_defun){
                  	$word2line->($trans->{defun_close});
									}else{
                  	$word2line->((($p)?$p:()));
									}
                  pop @paren_stack;
               }elsif(/\[/){
                  if(@word){
                     my ($w) = join('', @word);
                     $word2line->(' ', '(list-nth', ' ',  $w, ' ');
                     push @paren_stack, ']'; 
                  }else{
                    if($match_pattern){
                     $word2line->($trans->{'match_list_open'});
                     push @paren_stack, $trans->{'match_list_close'}; 
                    }else{
                     $word2line->($trans->{'list'});
                     push @paren_stack, ']]'; 
                   }
                  }
               }elsif(/\]/){
                  my $p = pop @paren_stack;
                  $word2line->($trans->{$p});
                  $wordcount++;
               }elsif(/\{/){
                  if(@word){
                     my ($w) = join('', @word);
                     @word = ();
                     $word2line->(' ', $trans->{'alist_get'}, ' ',  $w, ' ');
                     push @paren_stack, '{{';
                  }else{
                     $word2line->($trans->{'{'});
                     push @paren_stack, '{';
                  }
               }elsif(/\}/){
                  my $t = pop @paren_stack;

                  $word2line->($dialect->{trans}->{'}'});
						      $wordcount++;
               }elsif(/\,/){
                  $word2line->($dialect->{trans}->{','});
               }else{
                  push @word, $_
               }
               $linestarted = $_;
         }
      }
      $prev = $_;
   }
   if($codeline){
      $whitecheck->(1);
   }
	die "Err: lambda arglist still open on line $i" if  $lambda_open;


   #print 'll ' . Dumper   \@line;
   #die 'jsdf' . $lineparens;
   if($lambda_singleline_confirm){
      pop @line;
      pop @lineparens;
      pop @lineparens;
   }else{
      #$word2line->(')')
   }
   $word2line->( (@lineparens) ? @lineparens : ());

   my $oneword = 1 if $wordcount == 1;


   if(@line){
      push @lines, [ $indent, [@line], $instring, scalar(@paren_stack), $oneword, $post_doublepoint, $pre_doublepoint] ; 
   }else{
     if($comment){
      push @lines, [ undef, [$comment], undef, undef, undef, undef, undef] ; 
     }else{
      push @lines, [ undef, [' ' x $indent], $instring, scalar(@paren_stack), $oneword, $post_doublepoint, $pre_doublepoint] ; 
    }
    }
   @line = ();
  }

  return \@lines;
}


sub dump_lines{
  my ($lines) = @_;

  join("\n", map {
    join('', @$_)
  } @$lines);
}

sub cliffhanger {
  my($dialect, $trans, $indent_stack, $lines,$indent, $li_ln, $lila_ln, $last_oneword, $last_post_doublepoint) = @_;

  my $cnt = 0;
  while(my $is = pop @$indent_stack){
    $cnt++;
    my ($ind, $ln, $lila, $la_ow, $post_dp) = @$is;

    if($indent == $ind){
      $cnt-- if $last_oneword;
      $lines->[$li_ln]->[1] = 
         ($last_post_doublepoint)
            ? $lines->[$li_ln]->[1] . (($la_ow)? ((@$indent_stack)?'':')') :'')
            : $lines->[$li_ln]->[1] . (($la_ow)?')':')') . ')' x $cnt; 
      last;
    }elsif($indent > $ind){
      #echo 'jsdf' . $last_post_doublepoint;
      $lines->[$ln]->[1] = $lines->[$ln]->[1] . ' ' . (($last_post_doublepoint) ? $trans->{cliffhanger_open} : '(');
      $lines->[$lila_ln]->[1] = $lines->[$lila_ln]->[1] . (($last_oneword or $last_post_doublepoint)?'': ')' ) . 
         (($last_post_doublepoint) ? $trans->{cliffhanger_close} : ')' x $cnt);
      push @$indent_stack, [ $ind, $ln ]; 
      last;
    }
  }

}

sub calculate_closing_parens {
  my ($indent_stack) = @_;
  my $parens = 0;
  foreach (@$indent_stack){
   my ($last_indent, $li_ln, $lila_ln, $last_oneword, $post_doublepoint) = @$_;
   $parens++ unless $post_doublepoint;
 }
 return ')' x $parens;
}

sub parentie {
   my ($raw_lines, $dialect, $trans) = @_;
   my ($last_indent, $li_ln, $lila_ln);
   my ($last_parstack, $blockbuster) = (0,0);
   my $last_oneword;
   my @indent_stack;
   my ($pre_doublepoint, $doublepoint, $last_doublepoint, $doublepoint_off);
   # we want lines not to be 0 based, because the index
   # should reflect the line numbers in a file which begin at 1
   my (@lines) = ([]);

   my $i = 0;
   foreach (@$raw_lines) {
      $i++;
      my ($indent, $line, $instring, $parstack, $oneword, $post_dp, $pre_dp) = @$_;
      ($doublepoint, $doublepoint_off) = (1, 1) if  $pre_dp ; #ugly
      #echo ppd => $i , $doublepoint_off;
      if(defined $indent){
         if(defined $last_indent){
            if($last_parstack){
               ($lila_ln) = ($i);
            }else{
               if($indent > $last_indent){
                 #          echo llll => $last_doublepoint;
                  $lines[$li_ln]->[1] = '(' .  $lines[$li_ln]->[1] unless ($last_doublepoint); 
                  push @indent_stack, [ $last_indent, $li_ln, $lila_ln, $last_oneword, $last_doublepoint];
               }elsif($indent < $last_indent){
                  if($blockbuster){
                     $lines[$li_ln]->[1] = '(' .  $lines[$li_ln]->[1] unless $doublepoint; 
                  }else{
                     $lines[$li_ln]->[1] = '(' .  $lines[$li_ln]->[1] 
                        unless($last_oneword or $doublepoint);
                  }

                 cliffhanger($dialect, $trans, \@indent_stack, \@lines,$indent, $li_ln, $lila_ln, $last_oneword, $last_doublepoint);
                 #          echo ccccc => $i;
                 ($doublepoint_off, $last_doublepoint) = (undef, undef); 
               }else{
                  $lines[$li_ln]->[1] = '(' .  $lines[$li_ln]->[1] 
                     unless ($last_oneword or $last_doublepoint); 
                  if($blockbuster){
                     $lines[$lila_ln]->[1] = $lines[$lila_ln]->[1] . ')' unless ($last_oneword or $last_doublepoint);
                  }else{
                     $lines[$li_ln]->[1] = $lines[$li_ln]->[1] . ')' unless ($last_oneword or $last_doublepoint);
                  }
               }
               ($last_indent, $li_ln, $lila_ln) = ($indent, $i, $i);
            }
         }else{
            ($last_indent, $li_ln, $lila_ln) = (0, $i, $i);
         }
         push @lines, [ ' ' x $indent , join('', @$line)];
         $last_oneword = $oneword;
         $blockbuster = $last_parstack;
         $last_parstack = $parstack;
         $last_doublepoint = $doublepoint;
         ($doublepoint, ) = (1) if  $post_dp ; #ugly
         ($doublepoint_off, ) = (1) if  $pre_dp ; #ugly
      }else{
         push @lines, $line;
      }
   }


   if($blockbuster){
     if($last_parstack){
       die "Err: unclosed parens"
     }else{
      $lines[$li_ln]->[1] = '(' .  $lines[$li_ln]->[1] unless $last_oneword;
       $lila_ln = $li_ln;
       $li_ln = $i;
     }
   }else{
     if(defined $li_ln){
       #echo  ddd => $doublepoint_off;
      $lines[$li_ln]->[1] = '(' .  $lines[$li_ln]->[1] unless ($last_oneword or $doublepoint_off);
    }else{
    }
   }

   if(defined $li_ln){
     #echo llll => $doublepoint_off, ' ' , $doublepoint;
      my ($parens) = calculate_closing_parens(\@indent_stack);
      $lines[$li_ln]->[1] =  $lines[$li_ln]->[1] . (($last_oneword or $doublepoint_off)?'':')')  . $parens; 
   }

   shift  @lines;
	#die xxx => Dumper @lines;
   return \@lines;
}

sub usage {
	die "Usage: $0 --dialect LISP-dialect --pp PREPROC ARG\n";
}

sub main {
	use Getopt::Long qw(GetOptions);
 
	my ($dialect_opt, $pp_opt);
	GetOptions(
    'dialect=s' => \$dialect_opt,
    'pp=s' => \$pp_opt,

  ) or usage();
	
  my ($filename) =  @ARGV;
  usage() unless ($filename);
  open (my $fh, '<', $filename) || die "Err: no opening"; 


  my @lines = <$fh>;
  close $fh;
   my ($dialect, $trans);
   if (exists $Lisp_dialects{$dialect_opt}) {
      $dialect = $Lisp_dialects{$dialect_opt};
      $trans = $dialect->{trans};
   }else{
      die "Err: lisp dialect $dialect_opt doesn't exists";
   }
  my ($raw_lines) = lines_loop(\@lines, $dialect, $trans, $pp_opt);
  my ($lines) = parentie $raw_lines, $dialect, $trans;
   print join("\n", map { join('',  @$_ ) }  @$lines);
   print "\n";
  #print Dumper $raw_lines;
}

main; 
