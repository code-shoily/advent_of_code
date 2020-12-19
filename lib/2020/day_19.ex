defmodule AdventOfCode.Y2020.Day19 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/19
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 19

  @alphabet ~w/a b |/

  def run_1 do
    {rules, messages} = process(input!())

    regex = to_regex(:erlang.iolist_to_binary(rule_to_regex(rules, "0", "")))

    length(Enum.filter(messages, &Regex.match?(regex, &1)))
  end

  def process(input), do: parse(String.split(input, "\n\n"))

  def parse([rules, messages]), do: {parse_rules(rules), parse_messages(messages)}

  defp parse_rules(rules) do
    rules
    |> String.split("\n")
    |> Enum.map(&String.replace(&1, "\"", ""))
    |> Enum.map(fn rule ->
      [idx, rule] = String.split(rule, ":")
      {idx, String.trim(rule)}
    end)
    |> Enum.into(%{})
  end

  defp parse_messages(messages), do: String.split(messages, "\n")

  defp rule_to_regex(rules, idx, regex) do
    rules[idx]
    |> String.split(" ")
    |> Enum.map(&((&1 in @alphabet && &1) || ["(", rule_to_regex(rules, &1, regex), ")"]))
  end

  defp to_regex(source), do: %Regex{source: "^" <> source <> "$"}
end
