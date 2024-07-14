import { tangibleClient } from "./common.ts";

type Person = {
    name: string;
    nationality: string;
    age: number;
};

const tc = tangibleClient();

const example: Person = { name: "Jose", nationality: "Spain", age: 52 };
const response = await tc.expectJson("Give me 10 random people", [example]);

if (response.outcome === "Success") {
    console.log(response.value);
}