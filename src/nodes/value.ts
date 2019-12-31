import { Node } from './node';
import { NodeTag } from './tags';

declare interface ValUnion {
  ival: number;
  str: string;
}

export type TValueType = NodeTag.T_String | NodeTag.T_Integer | NodeTag.T_Float | NodeTag.T_BitString;

export interface Value<T extends TValueType> extends Node<T> {
  val: ValUnion;
}

export function makeInteger(i: number): Value<NodeTag.T_Integer> {
  return {
    type: NodeTag.T_Integer,
    val: {
      ival: i,
      str: ''
    }
  };
}

export function makeString(s: string): Value<NodeTag.T_String> {
  return {
    type: NodeTag.T_String,
    val: {
      ival: 0,
      str: s
    }
  };
}
