defmodule AdventOfCode.Y2020.Day07 do
  @moduledoc """
  --- Day 7: Handy Haversacks ---
  Problem Link: https://adventofcode.com/2020/day/7
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 7)

  def run(input \\ input()) do
    input = Map.new(Transformers.lines(input), &parse_rule/1)
    {ancestor(graph(input)), descendant(input) - 1}
  end

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

  defp ancestor(g, v \\ "shiny gold"), do: length(:digraph_utils.reaching([v], g)) - 1

  defp descendant(g, v \\ "shiny gold"),
    do: Enum.reduce(g[v] || [], 1, fn {n, v}, num -> num + n * descendant(g, v) end)
end
