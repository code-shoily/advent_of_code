defmodule AdventOfCode.Y2020.Day07 do
  @moduledoc """
  --- Day 7: Handy Haversacks ---
  Problem Link: https://adventofcode.com/2020/day/7
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 7

  def run_1, do: input!() |> parse() |> graph() |> ancestor_count("shiny gold")
  def run_2, do: (input!() |> parse() |> descendant_count("shiny gold")) - 1

  def parse(input), do: for(i <- String.split(input, "\n"), into: %{}, do: parse_rule(i))

  def parse_rule(rule) do
    line = Regex.named_captures(~r/(?<src>.+) bags contain (?<bags>.+)\./, rule)

    {line["src"],
     Regex.split(~r/bags?,?/, line["bags"], trim: true)
     |> Enum.map(fn s ->
       match = Regex.named_captures(~r/(?<n>no|\d+) (?<bag>.+)/, String.trim(s))
       {(match["n"] == "no" && 0) || String.to_integer(match["n"]), match["bag"]}
     end)}
  end

  defp graph(v), do: Enum.reduce(v, :digraph.new(), &connect(&2, &1))

  defp connect(g, {vi, connections}) do
    Enum.each(connections, fn {n, vf} ->
      :digraph.add_edge(g, :digraph.add_vertex(g, vi), :digraph.add_vertex(g, vf), n)
    end)

    g
  end

  defp ancestor_count(g, v), do: length(:digraph_utils.reaching([v], g)) - 1

  def descendant_count(g, v),
    do: Enum.reduce(g[v] || [], 1, fn {n, v}, num -> num + n * descendant_count(g, v) end)
end
