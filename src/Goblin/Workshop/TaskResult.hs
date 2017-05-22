module Goblin.Workshop.TaskResult where

data GoblinTaskError = GoblinTaskError
data UnifiedTaskError = UnifiedTaskError
data UnifiedTaskSuccess = UnifiedTaskSuccess

type TaskResult e r = Either GoblinTaskError (Either e r)
type UnifiedTaskResult = Either GoblinTaskError (Either UnifiedTaskError UnifiedTaskSuccess)

class UnifiableTaskError e where
  unifyError :: e -> UnifiedTaskError

class UnifiableTaskSuccess r where
  unifySuccess :: r -> UnifiedTaskSuccess

instance UnifiableTaskError a where
  unifyError _ = UnifiedTaskError

instance UnifiableTaskSuccess a where
  unifySuccess _ = UnifiedTaskSuccess
