defmodule AdventOfCode.Y2018.Day06Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1806

  alias AdventOfCode.Y2018.Day06, as: Solution

  @data "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"

  @points [
    {1, 1},
    {1, 6},
    {8, 3},
    {3, 4},
    {5, 5},
    {8, 9}
  ]

  describe "parse" do
    test "Parses input by creating a list of points from a set of strings" do
      assert Solution.parse(@data) == @points
    end
  end

  describe "get_corners/1" do
    test "Computes the extreme corners of a set of points" do
      assert Solution.get_corners(@points) == {{1, 8}, {1, 9}}
    end
  end

  describe "covers_most_points/3" do
    assert Solution.covers_most_points(@points) == 17
  end

  describe "covers_distances_within/3" do
    assert Solution.covers_distances_within(@points, 32) == 16
  end

  test "Year 2018, Day 6" do
    assert Solution.run() == {4284, 35_490}
  end
end
