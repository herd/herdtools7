AArch64 model descriptions.

  $ show_relation () {
  >   miaou_relation=$1
  >   shift
  >   miaou7 -q -set-libdir ../libdir -tex catdefinitions.tex \
  >     -show "$miaou_relation" "$@" aarch64.cat
  > }

  $ show_relation IFB-ob
  \expandafter{\MakeUppercase\IFBobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ctrl{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \IFB{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\pickctrldep{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \IFB{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\addr{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \IFB{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\pickaddrdep{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \IFB{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\pickaddrdep{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \expandafter{\MakeUppercase\tcib{E\textsubscript{3}}{E\textsubscript{4}}}.
      \item \expandafter{\MakeUppercase\trib{E\textsubscript{3}}{E\textsubscript{4}}}.
      \end{itemize}
    \item \IFB{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\DSBob{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \IFB{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \IFB{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTagR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\tcib{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \IFB{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \IFB{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTagR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\tcbefore{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \IFB{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation DSB-ob
  \expandafter{\MakeUppercase\DSBobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item One of the following applies:
      \begin{itemize}
      \item \M{E\textsubscript{1}}.
      \item \DCCVAU{E\textsubscript{1}}.
      \item \IC{E\textsubscript{1}}.
      \item \TLBI{E\textsubscript{1}}.
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DSBFULL{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\notthecase{one of the following applies}}:
      \begin{itemize}
      \item \ImpTTDM{E\textsubscript{2}}.
      \item \ImpInstrR{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ETSTwo{} or \ETSThree{}.
    \item One of the following applies:
      \begin{itemize}
      \item \M{E\textsubscript{1}}.
      \item \DCCVAU{E\textsubscript{1}}.
      \item \IC{E\textsubscript{1}}.
      \item \TLBI{E\textsubscript{1}}.
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DSBFULL{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \ImpTTDM{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item One of the following applies:
      \begin{itemize}
      \item All of the following apply:
        \begin{itemize}
        \item \ExpR{E\textsubscript{1}}.
        \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{1}}}}.
        \end{itemize}
      \item All of the following apply:
        \begin{itemize}
        \item \ImpTagR{E\textsubscript{1}}.
        \item \expandafter{\MakeUppercase\sameinstance{E\textsubscript{1}}{E\textsubscript{4}}}.
        \item \ExpR{E\textsubscript{4}}.
        \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{4}}}}.
        \end{itemize}
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DSBLD{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\notthecase{one of the following applies}}:
      \begin{itemize}
      \item \ImpTTDM{E\textsubscript{2}}.
      \item \ImpInstrR{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ETSTwo{} or \ETSThree{}.
    \item One of the following applies:
      \begin{itemize}
      \item All of the following apply:
        \begin{itemize}
        \item \ExpR{E\textsubscript{1}}.
        \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{1}}}}.
        \end{itemize}
      \item All of the following apply:
        \begin{itemize}
        \item \ImpTagR{E\textsubscript{1}}.
        \item \expandafter{\MakeUppercase\sameinstance{E\textsubscript{1}}{E\textsubscript{4}}}.
        \item \ExpR{E\textsubscript{4}}.
        \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{4}}}}.
        \end{itemize}
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DSBLD{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \ImpTTDM{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpW{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DSBST{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\notthecase{one of the following applies}}:
      \begin{itemize}
      \item \ImpTTDM{E\textsubscript{2}}.
      \item \ImpInstrR{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ETSTwo{} or \ETSThree{}.
    \item \ExpW{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DSBST{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \ImpTTDM{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation Exp-obs
  \expandafter{\MakeUppercase\Expobsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\rf{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\ext{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpM{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\ext{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpM{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation IC-ca
  \expandafter{\MakeUppercase\ICcaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \NotDIC{}.
    \item \NotIDC{}.
    \item \IC{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ICafter{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ImpInstrR{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \W{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\DCafter{E\textsubscript{4}}{E\textsubscript{2}}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \NotDIC{}.
    \item \IDC{}.
    \item \IC{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ICafter{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ImpInstrR{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \W{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \DIC{}.
    \item \IDC{}.
    \item \ImpInstrR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \W{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation IC-ob
  \expandafter{\MakeUppercase\ICobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpInstrR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \ImpInstrR{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\Instrreadob{E\textsubscript{3}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation Instr-obs
  \expandafter{\MakeUppercase\Instrobsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\rf{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ImpInstrR{E\textsubscript{2}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\ICafter{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \DCCVAU{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\DCafter{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \W{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \W{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\DCafter{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\ICca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation Instr-read-ordered-before
  \expandafter{\MakeUppercase\Instrreadobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\ICafter{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \IC{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\DSBob{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\ICafter{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \IC{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\IFBob{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \DIC{}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation TLBI-ca
  \expandafter{\MakeUppercase\TLBIcaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \TLBI{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\TLBIafter{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \ImpTTDR{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\ca{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \W{E\textsubscript{2}}.
  \end{itemize}

  $ show_relation TTD-in-same-scope
  \expandafter{\MakeUppercase\TTDinsamescopeemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\sameloworderbits{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \ExpM{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{2}}{E\textsubscript{4}}}.
    \item \ImpTTDR{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ImpTagR{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\sameloworderbits{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \ImpTagR{E\textsubscript{4}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{2}}{E\textsubscript{4}}}.
    \item \ImpTTDR{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation TLBI-ob
  \expandafter{\MakeUppercase\TLBIobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\TTDreadorderedbefore{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{3}}{E\textsubscript{1}}}.
    \item \expandafter{\MakeUppercase\TTDreadorderedbefore{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\ext{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\TTDinsamescope{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\sametranslationcontext{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\TTDreadorderedbefore{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\ext{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation TTD-obs
  \expandafter{\MakeUppercase\TTDobsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTD{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\rf{E\textsubscript{1}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\rf{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ImpTTD{E\textsubscript{2}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\TLBuncacheableca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\HUca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDW{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \W{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \W{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ImpTTDW{E\textsubscript{2}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\TLBIca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation TTD-read-ordered-before
  \expandafter{\MakeUppercase\TTDreadorderedbeforeemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\TLBIafter{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \TLBI{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\DSBob{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\TLBIafter{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \TLBI{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\IFBob{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation Tag-obs
  \expandafter{\MakeUppercase\Tagobsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpW{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\rf{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\ext{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ImpTagR{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTagR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\ext{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpW{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation addr
  \expandafter{\MakeUppercase\addremph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\dtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \Rreg{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\iiaddr{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpM{E\textsubscript{2}}.
    \item \ImpTagR{E\textsubscript{2}}.
    \item \ImpTTDM{E\textsubscript{2}}.
    \item \TLBI{E\textsubscript{2}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \item \ICIVAU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation aob
  \expandafter{\MakeUppercase\aobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\rmw{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpM{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\rmw{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\lmrs{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpRA{E\textsubscript{2}}.
      \item \ExpRQ{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\rmw{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ImpTTDW{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation bob
  \expandafter{\MakeUppercase\bobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item One of the following applies:
      \begin{itemize}
      \item \ExpM{E\textsubscript{1}}.
      \item \ImpTagR{E\textsubscript{1}}.
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DMBFULL{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpM{E\textsubscript{2}}.
      \item \ImpTagR{E\textsubscript{2}}.
      \item \MMUFAULT{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DMBFULL{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \DCCVAU{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DMBFULL{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \ExpM{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \DCCVAU{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DMBFULL{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item One of the following applies:
      \begin{itemize}
      \item All of the following apply:
        \begin{itemize}
        \item \ExpR{E\textsubscript{1}}.
        \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{1}}}}.
        \end{itemize}
      \item All of the following apply:
        \begin{itemize}
        \item \ImpTagR{E\textsubscript{1}}.
        \item \expandafter{\MakeUppercase\sameinstance{E\textsubscript{1}}{E\textsubscript{4}}}.
        \item \ExpR{E\textsubscript{4}}.
        \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{4}}}}.
        \end{itemize}
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DMBLD{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpM{E\textsubscript{2}}.
      \item \ImpTagR{E\textsubscript{2}}.
      \item \MMUFAULT{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpW{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DMBST{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \MMUFAULT{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpWL{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\amo{E\textsubscript{3}}{E\textsubscript{1}}}.
    \item \ExpRA{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpM{E\textsubscript{2}}.
      \item \ImpTagR{E\textsubscript{2}}.
      \item \MMUFAULT{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpWL{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpRA{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item One of the following applies:
      \begin{itemize}
      \item \ExpRA{E\textsubscript{1}}.
      \item \ExpRQ{E\textsubscript{1}}.
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpM{E\textsubscript{2}}.
      \item \ImpTagR{E\textsubscript{2}}.
      \item \MMUFAULT{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item One of the following applies:
      \begin{itemize}
      \item \ExpM{E\textsubscript{1}}.
      \item \ImpTagR{E\textsubscript{1}}.
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpWL{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation ctrl
  \expandafter{\MakeUppercase\ctrlemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\dtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \Rreg{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{4}}}.
  \item \BCC{E\textsubscript{4}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation data
  \expandafter{\MakeUppercase\dataemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\dtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \Rreg{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\iidata{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \ExpW{E\textsubscript{2}}.
  \end{itemize}

  $ show_relation dob
  \expandafter{\MakeUppercase\dobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\addr{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\data{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\ctrl{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \item \TLBI{E\textsubscript{2}}.
      \item \DCCVAU{E\textsubscript{2}}.
      \item \IC{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\addr{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\addr{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\lmrs{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpR{E\textsubscript{2}}.
      \item \ImpTagR{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\data{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\lmrs{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpR{E\textsubscript{2}}.
      \item \ImpTagR{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTagR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\tcbefore{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \end{itemize}

  $ show_relation dtrm
  \expandafter{\MakeUppercase\dtrmemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\notthecase{\rangelxsx{E\textsubscript{1}}}}.
    \item \expandafter{\MakeUppercase\lrrs{E\textsubscript{1}}{E\textsubscript{2}}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\lmrs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\dtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\dtrm{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation Exp-haz-ob
  \expandafter{\MakeUppercase\Exphazobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\sameloc{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \ExpR{E\textsubscript{3}}.
  \item One of the following applies:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\sca{E\textsubscript{3}}{E\textsubscript{4}}}.
    \item \expandafter{\MakeUppercase\sameEffect{E\textsubscript{3}}{E\textsubscript{4}}}.
    \end{itemize}
  \item \ExpR{E\textsubscript{4}}.
  \item \expandafter{\MakeUppercase\ca{E\textsubscript{4}}{E\textsubscript{5}}}.
  \item \expandafter{\MakeUppercase\ext{E\textsubscript{4}}{E\textsubscript{5}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpW{E\textsubscript{5}}.
    \item \ImpTTDW{E\textsubscript{5}}.
    \end{itemize}
  \item One of the following applies:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\sca{E\textsubscript{5}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\sameEffect{E\textsubscript{5}}{E\textsubscript{2}}}.
    \end{itemize}
  \item One of the following applies:
    \begin{itemize}
    \item \ExpW{E\textsubscript{2}}.
    \item \ImpTTDW{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation f-ib
  \expandafter{\MakeUppercase\fibemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpInstrR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \B{E\textsubscript{3}}.
  \item One of the following applies:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\iicoctrl{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item All of the following apply:
      \begin{itemize}
      \item \expandafter{\MakeUppercase\iicoctrl{E\textsubscript{3}}{E\textsubscript{4}}}.
      \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{4}}{E\textsubscript{2}}}.
      \end{itemize}
    \end{itemize}
  \end{itemize}

  $ show_relation haz-ob
  \expandafter{\MakeUppercase\hazobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\Exphazob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\TLBIob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\ICob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation hw-reqs
  \expandafter{\MakeUppercase\hwreqsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\localhwreqs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\hazob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation lob
  \expandafter{\MakeUppercase\lobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\introb{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\etsob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\fob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\posclob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\DSBob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\IFBob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\lmws{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\lmws{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\sca{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\dob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\pob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\aob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\bob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\lob{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\lob{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation local-hw-reqs
  \expandafter{\MakeUppercase\localhwreqsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\lob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\picklob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\localhwreqs{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\localhwreqs{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation ob
  \expandafter{\MakeUppercase\obemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\hwreqs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\obs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\ob{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\ob{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation obs
  \expandafter{\MakeUppercase\obsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \Expobs{E\textsubscript{1}}{either  E\textsubscript{2}} or \sca{an Effect which}{E\textsubscript{2}}.
  \item \Tagobs{E\textsubscript{1}}{either  E\textsubscript{2}} or \sca{an Effect which}{E\textsubscript{2}}.
  \item \expandafter{\MakeUppercase\TTDobs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\Instrobs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation pick-addr-dep
  \expandafter{\MakeUppercase\pickaddrdepemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\pickdtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \Rreg{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\iiaddr{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpM{E\textsubscript{2}}.
    \item \ImpTagR{E\textsubscript{2}}.
    \item \ImpTTDM{E\textsubscript{2}}.
    \item \TLBI{E\textsubscript{2}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \item \ICIVAU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation pick-basic-dep
  \expandafter{\MakeUppercase\pickbasicdepemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\pickdtrm{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation pick-ctrl-dep
  \expandafter{\MakeUppercase\pickctrldepemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\pickdtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \Rreg{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{4}}}.
  \item \BCC{E\textsubscript{4}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{2}}}.
  \end{itemize}

  $ show_relation pick-data-dep
  \expandafter{\MakeUppercase\pickdatadepemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\pickdtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \Rreg{E\textsubscript{3}}.
  \item One of the following applies:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item All of the following apply:
      \begin{itemize}
      \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{4}}}.
      \item \expandafter{\MakeUppercase\iicoctrl{E\textsubscript{4}}{E\textsubscript{2}}}.
      \end{itemize}
    \end{itemize}
  \item \expandafter{\MakeUppercase\iidata{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \ExpW{E\textsubscript{2}}.
  \end{itemize}

  $ show_relation pick-dep
  \expandafter{\MakeUppercase\pickdepemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item One of the following applies:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\pickbasicdep{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\pickaddrdep{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\pickdatadep{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\pickctrldep{E\textsubscript{1}}{E\textsubscript{2}}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\notthecase{\expandafter{\MakeUppercase\sameinstance{E\textsubscript{1}}{E\textsubscript{2}}}}}.
  \end{itemize}

  $ show_relation pick-dtrm
  \expandafter{\MakeUppercase\pickdtrmemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\dtrm{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\iicoctrl{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\pickdtrm{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\pickdtrm{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation pick-lob
  \expandafter{\MakeUppercase\picklobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\pickdep{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\lob{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \ExpW{E\textsubscript{2}}.
  \end{itemize}

  $ show_relation po-scl-ob
  \expandafter{\MakeUppercase\posclobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\scl{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \DCCVAU{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\scl{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpM{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \DCCVAU{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\scl{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation pob
  \expandafter{\MakeUppercase\pobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\pickaddrdep{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \item \TLBI{E\textsubscript{2}}.
      \item \DCCVAU{E\textsubscript{2}}.
      \item \IC{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item \expandafter{\MakeUppercase\pickdatadep{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\pickctrldep{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \item \TLBI{E\textsubscript{2}}.
      \item \DCCVAU{E\textsubscript{2}}.
      \item \IC{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\pickaddrdep{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ExpM{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \end{itemize}

  $ show_relation tc-ib
  \expandafter{\MakeUppercase\tcibemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\tcbefore{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\notthecase{\ExpR{E\textsubscript{2}}}}.
  \end{itemize}

  $ show_relation ii_direct_data
  \expandafter{\MakeUppercase\iidirectdataemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\notthecase{all of the following apply}}:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \M{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ show_relation tr-ib
  \expandafter{\MakeUppercase\tribemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpTTDR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\iidirectdata{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \B{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicoctrl{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpM{E\textsubscript{2}}.
    \item \ImpTagR{E\textsubscript{2}}.
    \item \MMUFAULT{E\textsubscript{2}}.
    \item \TagCheckFAULT{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation f-ob
  \expandafter{\MakeUppercase\fobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpInstrR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\notthecase{\ImpInstrR{E\textsubscript{2}}}}.
  \end{itemize}

  $ show_relation ets-ob
  \expandafter{\MakeUppercase\etsobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ETSTwo{}.
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \TLBUncacheableFAULT{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{2}}{E\textsubscript{3}}}.
    \item \ImpTTDR{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ETSThree{}.
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \MMUFAULT{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{2}}{E\textsubscript{3}}}.
    \item \ImpTTDR{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ETSThree{}.
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \TagCheckEXCENTRY{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\tcib{E\textsubscript{2}}{E\textsubscript{3}}}.
    \item \ImpTagR{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation tc-before
  \expandafter{\MakeUppercase\tcbeforeemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpTagR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \B{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicoctrl{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpM{E\textsubscript{2}}.
    \item \TagCheckFAULT{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation lrrs
  \expandafter{\MakeUppercase\lrrsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \Wreg{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\samegpr{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\notthecase{all of the following apply}}:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\samegpr{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \Wreg{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\samegpr{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item \Rreg{E\textsubscript{2}}.
  \end{itemize}

  $ show_relation lmrs
  \expandafter{\MakeUppercase\lmrsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \W{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\sameloc{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\notthecase{all of the following apply}}:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\sameloc{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \W{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\sameloc{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \item \R{E\textsubscript{2}}.
  \end{itemize}

  $ show_relation lmws
  \expandafter{\MakeUppercase\lmwsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item One of the following applies:
      \begin{itemize}
      \item \ExpM{E\textsubscript{1}}.
      \item \ImpTagR{E\textsubscript{1}}.
      \end{itemize}
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\sameloc{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpW{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\sameloc{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \ImpTTDW{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \end{itemize}

  $ show_relation same-loc
  \expandafter{\MakeUppercase\samelocemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \EvalidPA{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\notthecase{\TagM{E\textsubscript{1}}}}.
    \item \expandafter{\MakeUppercase\loc{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \EvalidPA{E\textsubscript{2}}.
    \item \expandafter{\MakeUppercase\notthecase{\TagM{E\textsubscript{2}}}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \TagM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\loc{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \TagM{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation TLBuncacheable-ca
  \expandafter{\MakeUppercase\TLBuncacheablecaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpTTDR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \TLBUncacheableFAULT{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpW{E\textsubscript{2}}.
    \item \ImpTTDW{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ show_relation HU-ca
  \expandafter{\MakeUppercase\HUcaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \ImpTTDW{E\textsubscript{2}}.
  \end{itemize}

