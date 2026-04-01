defmodule AdventOfCode.Y2020.Day17Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2017

  alias AdventOfCode.Y2020.Day17, as: Solution

  test "Year 2020, Day 17 run/1" do
    assert Solution.run() == {313, 2640}
  end
end
