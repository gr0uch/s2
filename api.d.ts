type S2Result<T extends object> = [proxy: T, fragment: DocumentFragment];

export function registerTemplate(
  name: string,
  template: string | Element
): Element;

export function parseMustache(html: string): Element;

export function html(
  template: TemplateStringsArray,
  ...props: Array<string | Element>
): Element;

export function observable<T extends object>(
  obj?: T,
  shouldPartiallyReplace?: boolean
): T;
export function computed<T extends object>(definition: T): T;
export function ref<T extends object>(obj: T): T;

interface S2Fn {
  <T extends object>(obj: T, template: Element): S2Result<T>;

  /**
   * Disabled by default, but can be enabled. This will automatically call
   * unmount when the DOM nodes mapped to an object are removed. This should
   * be disabled if you need to keep updating nodes that may be removed and
   * appended later.
   */
  shouldUnmountRoot: boolean;
  /**
   * (experimental): this will defer setting proxy values as a microtask. This
   * might be preferable if there is significant blocking in between updates.
   * However, it can break functionality in case there are updates that depend
   * on a previous update in the same tick. As long as the view model is not
   * read for updates (i.e. if only observable/computed is used) then this
   * should be safe.
   */
  isDeferred: boolean;
  /**
   * Set a different global object for server-side rendering.
   */
  window: Window | null;
  /**
   * Use comment nodes and turn on messages in the console. Warning: has a
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
