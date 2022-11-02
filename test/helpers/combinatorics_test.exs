defmodule AdventOfCode.Helpers.CombinatoricsTest do
  use ExUnit.Case
  @moduletag :combinatorics

  alias AdventOfCode.Helpers.Combinatorics

  doctest AdventOfCode.Helpers.Combinatorics

  describe "permutations/1" do
    test "permutations result in a list of size factorial" do
      assert length(Combinatorics.permutations(~w/a b/)) == 2
      assert length(Combinatorics.permutations([:a, :b, :c])) == 6
      assert length(Combinatorics.permutations([11, 22, 43, 44])) == 24
      assert length(Combinatorics.permutations([100, 423, 537, 434, 345])) == 120
    end
  end
end
