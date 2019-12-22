import { makeString, Value } from "./value";
import { NodeTag } from './tags';
import { Node } from './node';

export interface ColumnRef extends Node<NodeTag.T_ColumnRef> {
  fields: any[];
  location: number;
}

export interface ParamRef extends Node<NodeTag.T_ParamRef> {
	number: number;			/* the number of the parameter */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * A_Expr - infix, prefix, and postfix expressions
 */
export enum A_Expr_Kind
{
	AEXPR_OP,					/* normal operator */
	AEXPR_OP_ANY,				/* scalar op ANY (array) */
	AEXPR_OP_ALL,				/* scalar op ALL (array) */
	AEXPR_DISTINCT,				/* IS DISTINCT FROM - name must be "=" */
	AEXPR_NOT_DISTINCT,			/* IS NOT DISTINCT FROM - name must be "=" */
	AEXPR_NULLIF,				/* NULLIF - name must be "=" */
	AEXPR_OF,					/* IS [NOT] OF - name must be "=" or "<>" */
	AEXPR_IN,					/* [NOT] IN - name must be "=" or "<>" */
	AEXPR_LIKE,					/* [NOT] LIKE - name must be "~~" or "!~~" */
	AEXPR_ILIKE,				/* [NOT] ILIKE - name must be "~~*" or "!~~*" */
	AEXPR_SIMILAR,				/* [NOT] SIMILAR - name must be "~" or "!~" */
	AEXPR_BETWEEN,				/* name must be "BETWEEN" */
	AEXPR_NOT_BETWEEN,			/* name must be "NOT BETWEEN" */
	AEXPR_BETWEEN_SYM,			/* name must be "BETWEEN SYMMETRIC" */
	AEXPR_NOT_BETWEEN_SYM,		/* name must be "NOT BETWEEN SYMMETRIC" */
	AEXPR_PAREN					/* nameless dummy node for parentheses */
}

export interface A_Expr extends Node<NodeTag.T_A_Expr> {
	kind: A_Expr_Kind;		/* see above */
	name: Node<NodeTag.T_String>[];    			/* possibly-qualified name of operator */
	lexpr: Node<any>;		    	/* left argument, or NULL if none */
	rexpr: Node<any>;			    /* right argument, or NULL if none */
	location: number;		  /* token location, or -1 if unknown */
}

/*
 * A_Const - a literal constant
 */
export interface A_Const extends Node<NodeTag.T_A_Const> {
	val?: Value<any>;			/* value (includes type info, see value.h) */
	location: number;		/* token location, or -1 if unknown */
}
/*
 * TypeName - specifies a type in definitions
 *
 * For TypeName structures generated internally, it is often easier to
 * specify the type by OID than by name.  If "names" is NIL then the
 * actual type OID is given by typeOid, otherwise typeOid is unused.
 * Similarly, if "typmods" is NIL then the actual typmod is expected to
 * be prespecified in typemod, otherwise typemod is unused.
 *
 * If pct_type is true, then names is actually a field name and we look up
 * the type of that field.  Otherwise (the normal case), names is a type
 * name possibly qualified with schema and database name.
 */
export interface TypeName extends Node<NodeTag.T_TypeName> {
	names: Node<NodeTag.T_String>[];			/* qualified name (list of Value strings) */
	// Oid			typeOid;		/* type identified by OID */
	setof?: boolean;			/* is a set? */
	pct_type?: boolean;		/* %TYPE specified? */
	typmods: Node<any>[];		/* type modifier expression(s) */
	typemod: number;		/* prespecified type modifier */
	arrayBounds?: Node<any>[];	/* array bounds */
	location: number;		/* token location, or -1 if unknown */
};

/*
 * TypeCast - a CAST expression
 */
export interface TypeCast<T extends NodeTag> extends Node<NodeTag.T_TypeCast> {
	arg: Node<T>;			/* the expression being casted */
	typeName: TypeName;		/* the target type */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * A_Star - '*' representing all columns of a table or compound field
 *
 * This can appear within ColumnRef.fields, A_Indirection.indirection, and
 * ResTarget.indirection lists.
 */
export interface A_Star extends Node<NodeTag.T_A_Star>
{
}

/*
 * A_Indices - array subscript or slice bounds ([idx] or [lidx:uidx])
 *
 * In slice case, either or both of lidx and uidx can be NULL (omitted).
 * In non-slice case, uidx holds the single subscript and lidx is always NULL.
 */
export interface A_Indices extends Node<NodeTag.T_A_Indices>
{
	is_slice: boolean;		/* true if slice (i.e., colon present) */
	lidx: Node<any>;			/* slice lower bound, if any */
	uidx: Node<any>;			/* subscript, or slice upper bound if any */
}

/*
 * A_Indirection - select a field and/or array element from an expression
 *
 * The indirection list can contain A_Indices nodes (representing
 * subscripting), string Value nodes (representing field selection --- the
 * string value is the name of the field to select), and A_Star nodes
 * (representing selection of all fields of a composite type).
 * For example, a complex selection operation like
 *				(foo).field1[42][7].field2
 * would be represented with a single A_Indirection node having a 4-element
 * indirection list.
 *
 * Currently, A_Star must appear only as the last list element --- the grammar
 * is responsible for enforcing this!
 */
export interface A_Indirection extends Node<NodeTag.T_A_Indirection>
{
	arg: Node<any>;			        /* the thing being selected from */
	indirection: unknown[];	/* subscripts and/or field names and/or * */
}

/*
 * A_ArrayExpr - an ARRAY[] construct
 */
export interface A_ArrayExpr extends Node<NodeTag.T_ArrayExpr>
{
	elements: Node<any>[];		/* array element expressions */
	location: number;		      /* token location, or -1 if unknown */
}

export function makeColumnRef(colname: string, indirection: any[], location: number, yyscanner: unknown) {
  /*
   * Generate a ColumnRef node, with an A_Indirection node added if there
   * is any subscripting in the specified indirection list.  However,
   * any field selection at the start of the indirection list must be
   * transposed into the "fields" part of the ColumnRef node.
   */
  const c: ColumnRef = {
    type: NodeTag.T_ColumnRef,
    location,
    fields: Array.isArray(indirection) ? indirection : []
  };
  let nfields = 0;

  if (Array.isArray(indirection)) {
    // for(const l of indirection) {
    //     if (IsA(lfirst(l), A_Indices))
    //     {
    //         A_Indirection *i = makeNode(A_Indirection);
    //         if (nfields == 0)
    //         {
    //             /* easy case - all indirection goes to A_Indirection */
    //             c.fields = list_make1(makeString(colname));
    //             i.indirection = check_indirection(indirection, yyscanner);
    //         }
    //         else
    //         {
    //             /* got to split the list in two */
    //             i.indirection = check_indirection(list_copy_tail(indirection,
    //                                                             nfields),
    //                                             yyscanner);
    //             indirection = list_truncate(indirection, nfields);
    //             c.fields = lcons(makeString(colname), indirection);
    //         }
    //         i.arg = (Node *) c;
    //         return (Node *) i;
    //     }
    //     else if (IsA(lfirst(l), A_Star))
    //     {
    //         /* We only allow '*' at the end of a ColumnRef */
    //         if (lnext(indirection, l) != NULL)
    //             parser_yyerror("improper use of \"*\"");
    //     }
    //     nfields++;
    // }
  }

  /* No subscripting, so all indirection gets added to field list */
  c.fields.unshift(makeString(colname));

  return c;
}
