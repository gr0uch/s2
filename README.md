# s2-engine

![test status](https://github.com/gr0uch/s2/actions/workflows/test.yml/badge.svg)

s2 is a thin abstraction of the DOM that can be easily embedded on a page or within an existing framework, since it's toolless. It combines logic-less templates with reactive programming, and is a modern successor to [Knockout](https://github.com/knockout/knockout) or [Ember](https://github.com/emberjs/ember.js) with superior ergonomics and [performance](https://krausest.github.io/js-framework-benchmark/current.html).

At its core is a single function which provides the reactive data-binding. It also includes a Mustache-subset parser (`html`) and state management (`observable`, `computed`). See the <a href="https://gr0uch.github.io/s2/">documentation page</a> for usage details.


## Install

```sh
npm install s2-engine
```


## Example

```js
import bind, { html } from "https://esm.run/s2-engine";

const template = html`
  {{count}}
  <button onclick="{{increment}}">+</button>
`;

const viewModel = {
  count: 0,
  increment(event) {
    event.preventDefault();
    this.count++;
  }
};

const [proxy, fragment] = bind(viewModel, template);
document.body.appendChild(fragment);

// the viewModel can be interacted with using the proxy
window.proxy = proxy;
```

In its most basic form, it provides a reactive binding from data to a template. Changing the data updates the template.

With a modification of the above, a different object can be used as the data source:

```js
import { observable, computed } from "https://esm.run/s2-engine";

const source = observable({
  count: 0,
});

const viewModel = computed({
  count: () => source.count,
  increment: (event) => {
    event.preventDefault();
    source.count++;
  },
});
```

In the above example, the state has moved to `observable`, while `computed` provides the view. This provides an abstraction for arbitrary data to serve as the data model and propagating state to multiple views.

## Benchmarks

See [js-framework-benchmark results table](https://krausest.github.io/js-framework-benchmark/current.html).


## Development

s2 is written in the Parenscript subset of Common Lisp.

- [`parenscript-builder`](https://github.com/gr0uch/parenscript-builder): see instructions
- [`terser`](https://github.com/terser/terser): `npm i -g terser` to add this executable to `$PATH`

Need to build the `psbuild` binary from `parenscript-builder` and put it here to compile with `make`.


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
