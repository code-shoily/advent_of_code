defmodule AdventOfCode.Y2020.Day18 do
  @moduledoc """
  --- Day 18: Operation Order ---
  Problem Link: https://adventofcode.com/2020/day/18
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 18

  def run_1, do: solve(parse(input!(), false))
  def run_2, do: solve(parse(input!(), true))
  def parse(input, p), do: Enum.map(String.split(input, "\n"), &parse_rule(tokenize(&1), p))

  defp solve(expressions), do: Enum.sum(Enum.map(expressions, fn {d, _} -> eval(d) end))

  @ops %{"*" => {:op, :*}, "+" => {:op, :+}, "(" => :lp, ")" => :rp, " " => :sp}
  defp tokenize(expression) do
    String.graphemes(expression)
    |> Enum.map(&(Map.get(@ops, &1) || String.to_integer(&1)))
    |> Enum.reject(&(&1 == :sp))
  end

  defp parse_rule({lhs, tokens}, precedence), do: parse_rule(lhs, tokens, precedence)
  defp parse_rule(tokens, precedence), do: parse_rule(parse_term(tokens, precedence), precedence)

  defp parse_rule(lhs, tokens, precedence) do
    case tokens do
      [] ->
        {lhs, []}

      [:rp | _] ->
        {lhs, tokens}

      [{:op, op} | tokens] when op == :+ or not precedence ->
        {rhs, tokens} = parse_term(tokens, precedence)
        parse_rule({op, [lhs, rhs]}, tokens, precedence)

      [{:op, :*} | tokens] ->
        {rhs, tokens} = parse_term(tokens, precedence)
        {rhs, tokens} = parse_rule(rhs, tokens, precedence)
        {{:*, [lhs, rhs]}, tokens}
    end
  end

  defp parse_term(tokens, precedence) do
    case tokens do
      [:lp | tokens] ->
        {term, [:rp | tokens]} = parse_rule(tokens, precedence)
        {term, tokens}

      [number | tokens] when is_number(number) ->
        {number, tokens}
    end
  end

  defp eval({op, args}), do: apply(Kernel, op, Enum.map(args, &eval/1))
  defp eval(number) when is_integer(number), do: number
end
