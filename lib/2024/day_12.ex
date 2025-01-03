defmodule AdventOfCode.Y2024.Day12 do
  @moduledoc """
  --- Day 12: Garden Groups ---
  Problem Link: https://adventofcode.com/2024/day/12
  Difficulty: m
  Tags: geometry disjoint-set
  """
  alias AdventOfCode.Algorithms.DisjointSet
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 12)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    Enum.sum_by(input, fn {_, plants} ->
      plants |> Enum.sum_by(fn plant -> calculate_price(plant, &perimeters/1) end)
    end)
  end

  defp run_2(_input) do
    nil
  end

  def parse(data \\ input()),
    do: Transformers.lines(data) |> Enum.map(&String.graphemes/1) |> Grid.grid2d() |> plants()

  def plants(grid) do
    grid
    |> Enum.group_by(fn {_, plant} -> plant end, fn {grid, _} -> grid end)
    |> Map.new(fn {plant, locations} -> {plant, regions(locations)} end)
  end

  defp regions(plants) do
    Enum.reduce(plants, DisjointSet.new(plants), fn plant, region ->
      plant
      |> Grid.surrounding4()
      |> Enum.reduce(region, fn neighbour, region_acc ->
        region_acc
        |> DisjointSet.union(plant, neighbour)
      end)
    end)
    |> DisjointSet.components()
  end

  defp calculate_price(plant_set, multiply_by) do
    Enum.sum(multiply_by.(plant_set)) * Enum.count(plant_set)
  end

  defp perimeters(plant_set) do
    plant_set
    |> Enum.map(fn plant ->
      plant
      |> Grid.surrounding4()
      |> Enum.reject(&MapSet.member?(plant_set, &1))
      |> Enum.count()
    end)
  end
end
