\AgdaHide{
\begin{code}
module HeapDef where
open import AgdaDescription
\end{code}
}
\section{Тип данных для двоичной кучи}

\subsection{Заполнение дерева}
Куча заполняется элементами слева. Формализуем \cite{HeapFill} заполненность дерева:

\begin{definition}
Двоичное дерево высоты $h$ — \emph{полное} тогда и только тогда,
когда в нем $2^{h+1} - 1$ узлов.

Двоичное дерево высоты $h$ \emph{заполнено слева} тогда и только тогда,
когда выполняется ровно один из пунктов:
  \begin{itemize}
    \item дерево — пустое
    \item дерево — полное (то есть оба его поддерева — либо полные высоты $h-1$, либо пустые)
    \label{def:fill-nd}
    \item его левое поддерево — полное высотой $h-1$ и его правое поддерево — полное высотой $h-2$
    \item его левое поддерево — заполнено слева высотой $h-1$ и его правое поддерево — полное высотой $h-2$
    \item его левое поддерево — полное высотой $h-1$ и его правое поддерево — заполнено слева высотой $h-1$
  \end{itemize}
\end{definition}
Определим вспомогательный тип данных для обозначения заполненности дерева:
\begin{code}
data TreeState : Set where
  full almost : TreeState
\end{code}
Теперь, используя этот тип данных как дополнительный индекс и
индексируя дерево его высотой, задать тип данных для заполненного слева дерева
\begin{code}
data Tree : (h : ℕ) → TreeState → Set where
  et : Tree zero full -- Пустое дерево
  nf : ∀ {n} → (a : Tree n full) → (b : Tree n full)
      → Tree (succ n) full -- Полное дерево
  nd : ∀ {n} → (a : Tree (succ n) full) → (b : Tree n full)
      → Tree (succ (succ n)) almost -- Полные поддеревья разной высоты
  nl : ∀ {n} → (a : Tree (succ n) almost) → (b : Tree n full)
      → Tree (succ (succ n)) almost -- Правое поддерево — полное
  nr : ∀ {n} → (a : Tree n full) → (b : Tree n almost)
      → Tree (succ n) almost -- Левое поддерево — полное
\end{code}
Для удобства будем называть заполненное слева дерево, индексированное \DC{almost}
\emph{неполным}.

\section{Вставка элементов}
\label{sec:insert}

\subsection{Вставка элементов в полное дерево}
\label{sec:finsert}
\subsection{Вставка элементов в неполное дерево}
\label{sec:ainsert}

\section{Удаление элементов}
\label{sec:delete}
\subsection{Удаление из полного дерева}
\subsection{Удаление из неполного дерева}
