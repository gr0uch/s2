# s2

**s2 is a metaprogramming function that enables user interfaces to be mapped as data structures.**

It works by returning a `Proxy` so that plain JS objects (data + functions), can map directly to elements. It uses `Proxy` objects to bind data to the DOM, HTML `<template>` & `<slot>` tags, and `data-` attributes to bind data and events.


## Usage

Import the module:

```js
import s2 from "s2/dist/main.js";
```

Trivial example of composing templates, binding text and events:

```html
<template id="menu">
  <h1>Hello, world!</h1>
  <slot name="counter" data-template="#counter">Empty</slot>
</template>
<template id="counter">
  <span data=text="count"></span>
  <button data-event-click="increment">Increment</button>
</template>
```

Binding data:

```js
const template = document.getElementById("menu");
const [node, proxy] = s2({
  counter: {
    count: 0,
    increment() {
      this.count++;
    },
  }
}, template);
document.body.appendChild(node);
```

Finally: *any changes on the proxy object will reflect in the DOM!*

```js
proxy.counter.count = 1337; // DOM updates
```

Here are the data attributes it will look for:

```
data-text, data-event-*, data-unsafe-html
```


## Caveats

s2 relies on Proxy objects throughout, so one can not use references to the original objects and expect it to work. Instead, always get new references by accessing keys via the Proxy.


## Web Components

s2 is agnostic about most of [Web Components](https://developer.mozilla.org/en-US/docs/Web/Web_Components), since the specification doesn't play well with server-side rendered HTML without some non-trivial work. It aims to focus on orthogonal functionality like data binding.

Although s2 uses `<template>` and `<slot>` elements, it does so in a different context. For example, it is not possible to use the `slot` attribute to fill content in a slot, since s2 binds the slot to data rather than elements.


## Name

s2 can mean:
- simulacra 2: electric bugaloo
- super solenoid
- sneaky submarine
- season 2


## Development

Uses `parenscript-builder`, need to build the `psbuild` binary and put it here to compile with `make`. I couldn't figure out how to automate including this dependency yet.
