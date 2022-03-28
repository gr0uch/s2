# s2 Hot Module Replacement (HMR) Example

This is just an example of HMR that is decoupled from any build tool, although `esbuild` is given here as an example of how one would integrate with a build tool.

**Reading the source code is necessary to get an understanding of how this works, it contains useful comments. Start with `dev-client.mjs`.**

```
npm i
npm start
```

Then change the files in `src` dir.


## Context

HMR is tightly coupled to the runtime code that is being used, that is just by nature. It can also be coupled to the build tool, for example, there is a proposal [ESM-HMR](https://github.com/withastro/esm-hmr) which extends `import.meta` by the build tool.

What I set out to do here was to give an example that shows HMR doesn't require buy-in to any complex ecosystem of tooling, or lock-in to that ecosystem. The implementation given here is portable enough that one could add it to existing tools. The esbuild-specific code is contained in `dev-server.mjs`.


## Prior Art

- https://github.com/withastro/esm-hmr
- https://webpack.js.org/concepts/hot-module-replacement/
