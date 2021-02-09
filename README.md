# s2

Metaprogramming function for DOM manipulation. s2 enables interfaces to be written in a similar style as the DOM API itself by returning a `Proxy` so that plain JS objects (data + functions), can map directly to elements. It uses `Proxy` objects to bind data to the DOM, HTML `<template>` & `<slot>` tags, and `data-` attributes to bind data and events.


## Usage

Import the module:

```js
import s2 from "s2";
```

Defining a template:

```html
<template id="menu">
  <slot name="options" data-template="#option"></slot>
  <slot name="submit" data-template="#submit"></slot>
</template>
<template id="option">
  <button data-text="label" data-event-click="choose"></button>
</template>
<template id="submit">
  <button data-text="label" data-event-click="done"></button>
</template>
```

Binding data:

```js
const template = document.getElementById("menu");
const choose = () => {};
const [binding, element] = s2({
  options: [
    { label: "Red", choose },
    { label: "Blue", choose },
  ],
  submit: {
    label: "Submit",
    done: () => {},
  }
}, template);
document.body.appendChild(element);
```


## Name

s2 can mean:
- simulacra 2: electric bugaloo
- super solenoid
- sneaky submarine
- season 2


## Development

Uses `parenscript-builder`, need to build the `psbuild` binary and put it here to compile with `make`. I couldn't figure out how to automate including this dependency yet.
