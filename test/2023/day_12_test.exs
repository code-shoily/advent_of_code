defmodule AdventOfCode.Y2023.Day12Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2312

  alias AdventOfCode.Y2023.Day12, as: Solution

  test "Year 2023, Day 12 run/1" do
    assert Solution.run() == {7191, 6_512_849_198_636}
  end
end
