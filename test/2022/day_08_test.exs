defmodule AdventOfCode.Y2022.Day08Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2208

  alias AdventOfCode.Y2022.Day08, as: Solution

  test "Year 2022, Day 8 run/1" do
    assert Solution.run() == {1763, 671_160}
  end
end
