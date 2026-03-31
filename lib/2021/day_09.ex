defmodule AdventOfCode.Y2021.Day09 do
  @moduledoc """
  --- Day 9: Smoke Basin ---
  Problem Link: https://adventofcode.com/2021/day/9
  Difficulty: m
  Tags: graph connectivity grid
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Grid
  alias Yog.Connectivity

  def input, do: InputReader.read_from_file(2021, 9)

  def run(input \\ input()) do
    input = parse(input)
    grid = Grid.from_2d_list(input, :undirected, Grid.always())

    {risk_points(grid), basin_multiplier(grid)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  defp risk_points(grid) do
    graph = grid.graph
    rows = grid.rows
    cols = grid.cols

    0..(rows * cols - 1)
    |> Enum.reduce(0, fn id, acc ->
      height = Yog.Model.node(graph, id)
      neighbors = Yog.Model.neighbor_ids(graph, id)

      is_low =
        Enum.all?(neighbors, fn n_id ->
          n_height = Yog.Model.node(graph, n_id)
          n_height > height
        end)

      if is_low, do: acc + height + 1, else: acc
    end)
  end

  defp basin_multiplier(grid) do
    grid.graph
    |> Yog.filter_nodes(fn height -> height < 9 end)
    |> Connectivity.connected_components()
    |> Enum.map(&length/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.reduce(&Kernel.*/2)
  end
end
