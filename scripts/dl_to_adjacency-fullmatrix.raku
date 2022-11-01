#!/usr/bin/env raku

sub convert( $file ) {
    unless $file.IO.e { return };
    my $content = $file.IO.slurp(:enc("latin1"));
    return if $content ~~ /"ROW LABELS"/;
    my @blocks = $content.split(/":"\s+/);
    my @labels = @blocks[1].split(/\s+/)[0..*-2].map: *.trim;
    my $output =  " ," ~ @labels.join(",") ~ "\n";
    my $data-block = @blocks[2];
    for $data-block.split(/\v+\s*/)[0..*-2] -> $l {
        my @values = $l.trim.split(/\s+/);
        $output ~=  @labels.shift ~ ',' ~ @values.join(",") ~ "\n";
    }
    spurt( "$file.csv", $output);

}
for <grecia bulgaria croacia dinamarca francia inglaterra italia portugal
rusia suecia suiza>
-> $pais {
    convert( "data/$pais.dl");
}


