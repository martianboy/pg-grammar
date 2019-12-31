import { NodeTag } from "./tags";

export interface Node<T extends NodeTag> {
    type: T;
}

export function makeNode(type: NodeTag) {
    return { type };
}