defmodule AdventOfCode.Y2019.Day03Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1903

  alias AdventOfCode.Y2019.Day03, as: Solution

  test "Year 2019, Day 3" do
    assert Solution.run() == {1195, 91_518}
  end
end
