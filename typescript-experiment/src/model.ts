import { Message } from "./GptApiClient.ts";

type IntegerParam = {
    type: "integer";
    name: string;
};

type StringParam = {
    type: "string";
    name: string;
};

type EnumParam = {
    type: "enum";
    name: string;
    enum: string[];
};

type DoubleParam = {
    type: "double";
    name: string;
};

type BooleanParam = {
    type: "boolean";
    name: string;
};

export type Param =
    | IntegerParam
    | StringParam
    | EnumParam
    | BooleanParam
    | DoubleParam;

export type FunctionCall<I = string, O = string> = {
    name: string;
    description: string;
    params: Param[];
    function: (params: I) => Promise<O>;
};

export type ReasoningStrategy =
    | "Simple"
    | "ThinkStepByStep"
    | "SuggestMultipleAndPickOne";

export type TangibleResponse<T> =
    | TangibleResponseSuccess<T>
    | TangibleResponseFailure;

export type TangibleResponseSuccess<T> = {
    outcome: "Success";
    value: T;
    rawMessage: string;
    history: Message[];
};

export type TangibleResponseFailure = {
    outcome: "Failure";
    reason: string;
    rawMessage: string;
    history: Message[];
};

export type TangibleOptionResponseSuccess<T> = {
    outcome: "Success";
    value: T | null;
    rawMessage: string;
    history: Message[];
};

export type TangibleOptionResponse<T> =
    | TangibleOptionResponseSuccess<T>
    | TangibleResponseFailure;

export type TangibleEitherResponse<L, R> = {
    value: L | R;
    rawMessage: string;
    history: Message[];
};

type TextColumn = {
    columnType: "TextColumn";
    name: string;
};

type BooleanColumn = {
    columnType: "BooleanColumn";
    name: string;
};

type NumberColumn = {
    columnType: "NumberColumn";
    name: string;
};

type SingleChoiceColumn = {
    columnType: "SingleChoiceColumn";
    name: string;
    options: string[];
};

export type Column =
    | TextColumn
    | BooleanColumn
    | NumberColumn
    | SingleChoiceColumn;

export type TextCell = {
    cellType: "TextCell";
    value: string;
    column: Column;
};

export type BooleanCell = {
    cellType: "BooleanCell";
    value: boolean;
    column: Column;
};

export type NumberCell = {
    cellType: "NumberCell";
    value: number;
    column: Column;
};

export type SingleChoiceCell = {
    cellType: "SingleChoiceCell";
    value: string;
    column: Column;
};

export type Cell = TextCell | BooleanCell | NumberCell | SingleChoiceCell;

export type Row = {
    cells: Cell[];
};

export type Table = {
    columns: Column[];
    rows: Row[];
};

type RequestFunction = {
    name: string;
    description: string | null;
    parameters: Parameters;
};

type Parameters = {
    type: "object";
    properties: any;
};

export type ItemGroup = {
    name: string;
    items: string[];
};

type JSONValue = string | number | boolean | null | JSONObject | JSONArray;
type JSONObject = { [key: string]: JSONValue };
type JSONArray = JSONValue[];
export type JSONSerializable = JSONObject | JSONArray | JSONValue;
