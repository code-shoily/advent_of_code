defmodule AdventOfCode.Y2016.Day06Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1606

  alias AdventOfCode.Y2016.Day06, as: Solution

  test "Year 2016, Day 6" do
    assert Solution.run() == {"qzedlxso", "ucmifjae"}
  end
end
