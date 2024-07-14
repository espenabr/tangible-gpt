import { FunctionCall } from "../model.ts";
import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

type SumParams = {
    a: number;
    b: number;
};

const sum = (params: SumParams) => params.a + params.b;

const functionCalls: FunctionCall<SumParams, number>[] = [
    {
        name: "sum",
        description: "Sum of two integers",
        params: [
            { type: "integer", name: "a"},
            { type: "integer", name: "b"},
        ],
        function: params => Promise.resolve(sum(params))
    }
];

const response = await tc.expectNumber("What is 423784 + 3478383?", [], functionCalls);
if (response.outcome === "Success") {
    console.log(response.value);
}