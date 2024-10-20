defmodule AdventOfCode.Y2023.Day06Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2306

  alias AdventOfCode.Y2023.Day06, as: Solution

  test "Year 2023, Day 6 run/1" do
    assert Solution.run() == {440_000, 26_187_338}
  end
end
