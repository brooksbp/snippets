import std.stdio;
import std.string;
import std.algorithm;

void main()
{
    uint[string] freqs;

    foreach (line; stdin.byLine())
        foreach (word; split(strip(line)))
            ++freqs[word.idup];

    string[] words = freqs.keys;
    sort!((a,b) { return freqs[a] > freqs[b];})(words);

    // sort!(<compile-time args>)(<runtime args>);

    foreach (word; words)
        writefln("%6u\t%s", freqs[word], word);
}
