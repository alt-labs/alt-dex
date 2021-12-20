import { O } from '@mobily/ts-belt';

export type Option<T> = O.Option<T>;

export const Some = O.Some;
export const None = O.None;

export const mapWithDefault = O.mapWithDefault;
