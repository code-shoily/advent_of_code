defmodule AdventOfCode.Y2015.Day06Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1506

  alias AdventOfCode.Y2015.Day06, as: Solution

  test "Year 2015, Day 6" do
    assert Solution.run() == {377_891, 14_110_788}
  end
end
