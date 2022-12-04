defmodule AdventOfCode.Y2022.Day04Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2204

  alias AdventOfCode.Y2022.Day04, as: Solution

  test "Year 2022, Day 4 run/1" do
    assert Solution.run() == {518, 909}
  end
end
