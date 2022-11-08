#!/usr/bin/env raku

sub convert( $file ) {
    unless $file.IO.e { return };
    say $file;
    my @blocks = $file.IO.slurp(:enc("latin1")).split(/":"/);
    my @rows = @blocks[1].split(/\s+/)[1..*-2].map: *.trim;
    my @columns = @blocks[2].split(/\s+/)[1..*-2].map: *.trim;

    my $output =  " ," ~ @columns.join(",") ~ "\n";
    my $data-block;
    if @blocks[3] ~~ /DATA/ {
        $data-block = @blocks[4];
    } else {
        $data-block = @blocks[3];
    }
    for $data-block.split(/\v+/)[1..*-2] -> $l {
        my @values = $l.split(/\s+/);
        $output ~=  @rows.shift ~ @values.join(",") ~ "\n";
    }
    spurt( "$file.csv", $output);

}
for <spain-3> -> $pais {
    convert("data/euro-2004/$pais.dl");
    for 2..6 {
        convert( "data/euro-2004/$pais-$_.dl")
    }
}


