type PrimitiveKey = string | number | symbol;

interface DataObject {
  [key: PrimitiveKey]: any;
}

// Mainly for annotation purposes.
interface Proxy {
  [key: PrimitiveKey]: any;
}

type S2Result = [
  proxy: Proxy,
  fragment: DocumentFragment,
];

export function registerTemplate(
  name: string,
  template: string | Element
): Element;

export function parseMustache(html: string): Element;

export function html(
  template: TemplateStringsArray,
  ...props: Array<string | Element>
): Element;

export function observable(
  obj?: DataObject,
  shouldPartiallyReplace?: boolean
): Proxy;
export function computed(definition: DataObject): DataObject;
export function ref(obj: DataObject): DataObject;

type S2Fn = ((obj: DataObject, template: Element) => S2Result) & {
  /**
   * enabled by default, but can be disabled. This will automatically call
   * unmount when the DOM nodes mapped to an object are removed. This should
   * only be disabled if you need to keep updating nodes that may be removed and
   * appended later.
   */
  shouldUnmountRoot: boolean;
  /**
   * (experimental): this will defer setting proxy values as a microtask. This
   * might be preferable if there is significant blocking in between updates.
   * However, it can break functionality in case there are updates that depend
   * on a previous update in the same tick.
   */
  isDeferred: boolean;
  /**
   * set a different global object for server-side rendering
   */
  window: Window | null;
  /**
   * use comment nodes and turn on messages in the console. Warning: has a
   * performance impact.
   */
  debug: boolean;
};

export default function s2(obj: DataObject, template: Element): S2Result;

export const root: symbol;
export const target: symbol;
export const mount: symbol;
export const unmount: symbol;
export const move: symbol;
