type S2Result<T> = [proxy: T, fragment: DocumentFragment];

export function registerTemplate(
  name: string,
  template: string | Element
): Element;

export function parseMustache(html: string): Element;

export function html(
  template: TemplateStringsArray,
  ...props: Array<string | Element>
): Element;

export function observable<T>(
  obj?: T,
  shouldPartiallyReplace?: boolean
): T;
export function computed<T>(definition: T): T;
export function ref<T>(obj: T): T;

interface S2Fn {
  <T>(obj: T, template: Element): S2Result<T>;

  /**
   * disabled by default, but can be enabled. This will automatically call
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
}

declare const s2: S2Fn;
export default s2;

export const root: symbol;
export const target: symbol;
export const mount: symbol;
export const unmount: symbol;
export const move: symbol;
