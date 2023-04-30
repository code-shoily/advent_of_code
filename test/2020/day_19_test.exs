defmodule AdventOfCode.Y2020.Day19Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2019

  alias AdventOfCode.Y2020.Day19, as: Solution

  test "Year 2020, Day 19" do
    assert Solution.run() == {248, 381}
  end
end
