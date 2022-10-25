#!/usr/bin/env raku

sub convert( $file ) {
    unless $file.IO.e { return };
    my @blocks = $file.IO.slurp(:enc("latin1")).split(/":"/);
    my @rows = @blocks[1].split(/\s+/)[1..*-3].map: *.trim;
    my @columns = @blocks[2].split(/\s+/)[1..*-2].map: *.trim;

    my $output =  " ," ~ @columns.join(",") ~ "\n";
    for @blocks[3].split(/\v/)[1..*-2] -> $l {
        my @values = $l.split(/\s+/);
        $output ~=  @rows.shift ~ @values.join(",") ~ "\n";
    }
    spurt( "$file.csv", $output);

}
for <alemania chequia> -> $pais {
    convert( "data/$pais.dl");
    for 2..6 {
        convert( "data/$pais-$_.dl")
    }
}


