# s²

**s2 is a function for reactive web UI.**

See the <a href="https://gr0uch.github.io/s2/">documentation page</a> for usage details.


## Benchmarks

See [js-framework-benchmark results table](https://krausest.github.io/js-framework-benchmark/current.html).


## Flags

- `s2.debug`: turn on messages in the console. Warning: has a performance impact.
- `s2.isDeferred` (experimental): this will defer setting proxy values until the next frame. This might be preferable if there is significant blocking in between updates. However, it can break functionality in case there are updates that depend on a previous update in the same tick.
- `s2.window`: set a different global object for server-side rendering.


## Name

s2 can mean:
- simulacra 2: electric bugaloo
- super solenoid
- sneaky submarine
- season 2
- 2 kilobytes compressed


## Development

s2 is written in the Parenscript subset of Common Lisp.

- [`parenscript-builder`](https://github.com/gr0uch/parenscript-builder): see instructions
- [`terser`](https://github.com/terser/terser): `npm i -g terser` to add this executable to `$PATH`

Need to build the `psbuild` binary from `parenscript-builder` and put it here to compile with `make`. I couldn't figure out how to automate including this dependency yet.


## Testing

Run a web server like `http-server .` and then navigate to the `/test/` directory. HTTP is required for loading modules.


## License

[BSD 3-Clause](https://github.com/gr0uch/s2/blob/master/LICENSE)
