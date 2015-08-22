use strict;

my %args = @ARGV;
my @x; 

for(my $i = 0; $i <$args{"dim"}*$args{"dim"}; $i++){
    $x[$i] = 0;
}

my $cntr = 0;

while($cntr < $args{"cntr"}){
    my $raw = int(rand($args{"dim"}));
    my $col = int(rand($args{"dim"}));
    my $idx1 = $raw*$args{"dim"}+$col;
    my $idx2 = $col*$args{"dim"}+$raw;
    if($idx1 != $idx2 && $x[$idx1] == 0){
        $cntr++;
        my $rand = int(rand($args{"range"}))+1;
        $x[$idx1]=($x[$idx2]=$rand);
    }
}

my $fun = "(\\x y -> (map fromInteger [";
$fun .= $x[0];
for(my $i = 1; $i < $args{"dim"}*$args{"dim"}; $i++){
$fun .= " ,".$x[$i];}
$fun .=  "])!!(x*".$args{"dim"}."+y))";

print "----\n";
print "clusterize ".$fun." ".$args{"maxc"}." [0..".($args{"dim"}-1)."]\n"; 
print "calcobj ".$fun." \n";
print "----\n";

print "----\n";
print "n=".$args{"dim"}.";\n";
print "total=".$args{"maxc"}.";\n";
print "simmilarities=\n[";
for( my $j = 0; $j < $args{"dim"}; $j++){
    print "|".$x[$j*$args{"dim"}];
    for(my $i = 1; $i < $args{"dim"}; $i++){
        print ", ".$x[$j*$args{"dim"}+$i];
    }
    print "\n";
}
print "|];\n";

print "----\n";