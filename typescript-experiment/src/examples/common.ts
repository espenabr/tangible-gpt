import TangibleClient from "../TangibleClient.ts";

const OPEN_AI_KEY = "<insert key here>";

export const tangibleClient = () => new TangibleClient(OPEN_AI_KEY);
