defmodule AdventOfCode.Y2015.Day22Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1522

  alias AdventOfCode.Y2015.Day22, as: Solution

  test "Year 2015, Day 22 run/1" do
    assert Solution.run() == {1269, 1309}
  end
end
