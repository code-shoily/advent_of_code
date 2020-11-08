defmodule AdventOfCode.Y2018.Day6Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y186

  alias AdventOfCode.Y2018.Day6, as: Solution

  @data "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"

  @points [
    {1, 1},
    {1, 6},
    {8, 3},
    {3, 4},
    {5, 5},
    {8, 9}
  ]

  describe "process" do
    test "Processes input by creating a list of points from a set of strings" do
      assert Solution.process(@data) == @points
    end
  end

  describe "manhattan_distance" do
    test "Computes manhattan distance between origin and another" do
      assert Solution.manhattan_distance({0, 0}, {10, 20}) == 30
    end

    test "Computes manhattan distance when there is no negative" do
      assert Solution.manhattan_distance({100, 50}, {10, 10}) == 130
    end

    test "Computes manhattan distance when there is no positive" do
      assert Solution.manhattan_distance({15, 17}, {16, 19}) == 3
    end
  end

  describe "get_corners/1" do
    test "Computes the extreme corners of a set of points" do
      assert Solution.get_corners(@points) == {{1, 1}, {8, 9}}
    end
  end

  describe "create_world/1" do
    test "Valid world is generated" do
      corners = Solution.get_corners(@points)
      world = Solution.create_world(corners)
      keys = Map.keys(world)

      assert length(keys) == 72

      assert keys |> Enum.map(&elem(&1, 0)) |> Enum.uniq() |> Enum.sort() ==
               Enum.into(1..8, [])

      assert keys |> Enum.map(&elem(&1, 1)) |> Enum.uniq() |> Enum.sort() ==
               Enum.into(1..9, [])

      assert world |> Map.values() |> Enum.sum() == 0
    end
  end

  describe "remove_edges/1" do
    test "edges are removed from the points" do
      corners = Solution.get_corners(@points)
      assert Solution.remove_edges(@points, corners) == [{3, 4}, {5, 5}]
    end
  end

  describe "nearest_area/1" do
    test "Finds the nearest area" do
      areas = %{7 => 10, 1 => 11, 2 => 15}
      assert Solution.get_nearest_area(areas) == 7
    end

    test "Returns 0 for tied nearest area" do
      areas = %{0 => 10, 1 => 10, 2 => 15}
      assert Solution.get_nearest_area(areas) == 0
    end
  end

  # test "Year 2018, Day 6, Part 1" do
  #   assert Solution.run_1() == nil
  # end

  # test "Year 2018, Day 6, Part 2" do
  #   assert Solution.run_2() == nil
  # end
end
