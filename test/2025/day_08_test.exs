defmodule AdventOfCode.Y2025.Day08Test do
  use ExUnit.Case

  alias AdventOfCode.Y2025.Day08, as: Solution
  @moduletag :y2508

  @example """
  162,817,812
  57,618,57
  906,360,560
  592,479,940
  352,342,300
  466,668,158
  542,29,236
  431,825,988
  739,650,466
  52,470,668
  216,146,977
  819,987,18
  117,168,530
  805,96,715
  346,949,466
  970,615,88
  941,993,340
  862,61,35
  984,92,344
  425,690,689
  """

  test "Part 1 example with 10 connections" do
    coords = Solution.parse(@example)
    edges = Solution.build_sorted_edges(coords)
    n = tuple_size(coords)

    dsu =
      Enum.reduce(0..(n - 1), Yog.DisjointSet.new(), fn i, acc -> Yog.DisjointSet.add(acc, i) end)

    dsu =
      Enum.reduce(Enum.take(edges, 10), dsu, fn {i, j, _}, acc ->
        Yog.DisjointSet.union(acc, i, j)
      end)

    sizes = dsu |> Yog.DisjointSet.to_lists() |> Enum.map(&length/1) |> Enum.sort(:desc)
    assert Enum.product(Enum.take(sizes, 3)) == 40
  end

  test "Part 2 example" do
    coords = Solution.parse(@example)
    assert Solution.run_2(coords) == 25272
  end

  test "Year 2025, Day 8 run/1" do
    assert Solution.run() == {175_500, 6_934_702_555}
  end
end
