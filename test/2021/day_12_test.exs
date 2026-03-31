defmodule AdventOfCode.Y2021.Day12Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2112

  alias AdventOfCode.Y2021.Day12, as: Solution

  test "Year 2021, Day 12 run/1" do
    assert Solution.run() == {4659, 148_962}
  end
end
