import './index';

declare interface ListCell {
	ptr_value: any;
	int_value: number;
	oid_value: number;
}

declare interface List extends Node {
    length: number;
    max_length: number;
    elements: any[];
    initial_elements?: any[];
}