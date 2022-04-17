# s²

![test status](https://github.com/gr0uch/s2/actions/workflows/test.yml/badge.svg)

**s2 is a function for reactive web UI.**

See the <a href="https://gr0uch.github.io/s2/">documentation page</a> for usage details.


## Benchmarks

See [js-framework-benchmark results table](https://krausest.github.io/js-framework-benchmark/current.html).


## Flags

- `s2.shouldUnmountRoot`: enabled by default, but can be disabled. This will automatically call `unmount` when the DOM nodes mapped to an object are removed. This should only be disabled if you need to keep updating nodes that may be removed and appended later.
- `s2.isDeferred` (experimental): this will defer setting proxy values as a microtask. This might be preferable if there is significant blocking in between updates. However, it can break functionality in case there are updates that depend on a previous update in the same tick.
- `s2.window`: set a different global object for server-side rendering.
- `s2.debug`: turn on messages in the console. Warning: has a performance impact.


## Development

s2 is written in the Parenscript subset of Common Lisp.

- [`parenscript-builder`](https://github.com/gr0uch/parenscript-builder): see instructions
- [`terser`](https://github.com/terser/terser): `npm i -g terser` to add this executable to `$PATH`

Need to build the `psbuild` binary from `parenscript-builder` and put it here to compile with `make`. I couldn't figure out how to automate including this dependency yet.


## Testing

Run automated tests with Deno:

```sh
make && deno test test/
```

### Manual testing pages

Run a web server like `http-server .` and then navigate to the `/test/` directory. HTTP is required for loading modules.


## Name

s2 is short for simulacra 2. Prior art: I wrote [a similar library that is limited to ES5](https://github.com/gr0uch/simulacra), so no proxies.


## License

[BSD 3-Clause](https://github.com/gr0uch/s2/blob/master/LICENSE)
