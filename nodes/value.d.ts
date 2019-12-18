import './index';

declare interface ValUnion {
    ival: number;
    str: string;
}

declare interface Value extends Node {
    val: ValUnion;
}