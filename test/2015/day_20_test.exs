defmodule AdventOfCode.Y2015.Day20Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y1520

  alias AdventOfCode.Y2015.Day20, as: Solution

  @tag :skip_slow
  test "Year 2015, Day 20 run/1" do
    assert Solution.run() == {786_240, 831_600}
  end
end
