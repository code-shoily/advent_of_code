defmodule AdventOfCode.Y2020.Day19 do
  @moduledoc """
  --- Day 19: Monster Messages ---
  Problem Link: https://adventofcode.com/2020/day/19
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 19)

  def run(input \\ input()), do: {run_1(input), run_2(input)}
  def run_1(input), do: match_count(parse(input, false))
  def run_2(input), do: match_count(parse(input, true))

  def parse(input, loop?) do
    {rules, messages} =
      input
      |> String.split(~r{(\r\n\r\n|\r\r|\n\n)})
      |> parse_paragraph()
      |> maybe_override(loop?)

    {
      messages,
      rules
      |> to_re("0", "", loop?)
      |> :erlang.iolist_to_binary()
      |> to_re()
    }
  end

  defp maybe_override(parsed, loop?),
    do: (loop? && override_8_11(parsed)) || Function.identity(parsed)

  @overrides %{"8" => "42 +", "11" => "42 31 | 42 11 31"}
  defp override_8_11({rules, messages}), do: {Map.merge(rules, @overrides), messages}
  defp parse_paragraph([rules, messages]), do: {parse_rules(rules), parse_messages(messages)}
  defp parse_messages(messages), do: Transformers.lines(messages)

  defp parse_rules(rules) do
    for rule <- Transformers.lines(rules), into: %{} do
      [idx, rule] = String.split(String.replace(rule, "\"", ""), ":")
      {idx, String.trim(rule)}
    end
  end

  defp to_re(rules, "11", re, true),
    do: ["(?<m>(#{to_re(rules, "42", re, true)})(?&m)?(#{to_re(rules, "31", re, true)}))"]

  @reserved ~w/a b | +/
  defp to_re(rules, idx, re, loop?) do
    rules[idx]
    |> String.split(" ")
    |> Enum.map(&((&1 in @reserved && &1) || ["(", to_re(rules, &1, re, loop?), ")"]))
  end

  defp to_re(source), do: %Regex{source: splat("^" <> source <> "$")}
  defp splat(re), do: Enum.reduce(@reserved, re, &String.replace(&2, "(#{&1})", &1))
  defp match_count({messages, re}), do: length(Enum.filter(messages, &Regex.match?(re, &1)))
end
