import { Column, Table } from "../model.ts";
import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const seaAnimalColumn: Column = {
    columnType: "TextColumn",
    name: "Sea animal",
};
const animalTypeColumn: Column = {
    columnType: "EnumColumn",
    name: "Type",
    options: ["Fish", "Shellfish", "Jellyfish"],
};

const originalColumns: Column[] = [seaAnimalColumn, animalTypeColumn];

const originalTable: Table = {
    columns: originalColumns,
    rows: [
        {
            cells: [
                { cellType: "TextCell", value: "Cod", column: seaAnimalColumn },
                {
                    cellType: "SingleChoiceCell",
                    value: "Fish",
                    column: animalTypeColumn,
                },
            ],
        },
        {
            cells: [
                {
                    cellType: "TextCell",
                    value: "Salmon",
                    column: seaAnimalColumn,
                },
                {
                    cellType: "SingleChoiceCell",
                    value: "Fish",
                    column: animalTypeColumn,
                },
            ],
        },
        {
            cells: [
                {
                    cellType: "TextCell",
                    value: "Crab",
                    column: seaAnimalColumn,
                },
                {
                    cellType: "SingleChoiceCell",
                    value: "Shellfish",
                    column: animalTypeColumn,
                },
            ],
        },
        {
            cells: [
                {
                    cellType: "TextCell",
                    value: "Lobster",
                    column: seaAnimalColumn,
                },
                {
                    cellType: "SingleChoiceCell",
                    value: "Shellfish",
                    column: animalTypeColumn,
                },
            ],
        },
    ],
};

const addedColumnResponse = await tc.expectTableWithAddedColumn(
    { columnType: "NumberColumn", name: "Average weight" },
    "Average weight (kg)",
    originalTable,
);
if (addedColumnResponse.outcome === "Success") {
    console.log(addedColumnResponse.value);
}

//const addedRowResponse = await tc.expectTableWithAddedRow(
//    originalTable,
//    "Another random sea animal",
//);
//if (addedRowResponse.outcome === "Success") {
//    console.log(addedRowResponse.value);
//}