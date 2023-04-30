defmodule AdventOfCode.Y2019.Day04Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1904

  alias AdventOfCode.Y2019.Day04, as: Solution

  test "Year 2019, Day 4" do
    assert Solution.run() == {1099, 710}
  end
end
