defmodule AdventOfCode.Y2020.Day04Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2004

  alias AdventOfCode.Y2020.Day04, as: Solution

  test "Year 2020, Day 4" do
    assert Solution.run() == {233, 111}
  end
end
