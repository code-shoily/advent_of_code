defmodule AdventOfCode.Y2020.Day16Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2016

  alias AdventOfCode.Y2020.Day16, as: Solution

  test "Year 2020, Day 16" do
    assert Solution.run() == {32_835, {:todo, 2}}
  end
end
