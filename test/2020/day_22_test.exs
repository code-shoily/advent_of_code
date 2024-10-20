defmodule AdventOfCode.Y2020.Day22Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2022

  alias AdventOfCode.Y2020.Day22, as: Solution

  test "Year 2020, Day 22, Part 1" do
    assert Solution.run() == {32_162, 32_534}
  end
end
