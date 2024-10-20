defmodule AdventOfCode.Y2021.Day04Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2104

  alias AdventOfCode.Y2021.Day04, as: Solution

  test "Year 2021, Day 4" do
    assert Solution.run() == {11_774, 4495}
  end
end
