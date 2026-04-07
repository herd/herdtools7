  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show IFB-ob aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show DSB-ob aarch64.cat
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
    \item \Variant{ETS2} or \Variant{ETS3}.
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
      \item \ImpTagR{E\textsubscript{1}}.
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
    \item \Variant{ETS2} or \Variant{ETS3}.
    \item \ExpR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{1}}}}.
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
    \item \Variant{ETS2} or \Variant{ETS3}.
    \item \ExpW{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \DSBST{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \ImpTTDM{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show Exp-obs aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show IC-ca aarch64.cat
  \expandafter{\MakeUppercase\ICcaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \NotVariant{DIC} and \NotVariant{IDC}.
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
    \item \NotVariant{DIC} and \Variant{IDC}.
    \item \IC{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ICafter{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \ImpInstrR{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \W{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \Variant{DIC} and \Variant{IDC}.
    \item \ImpInstrR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \W{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show IC-ob aarch64.cat
  \expandafter{\MakeUppercase\ICobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpInstrR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \ImpInstrR{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\Instrreadob{E\textsubscript{3}}{E\textsubscript{2}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show Instr-obs aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show Instr-read-ordered-before aarch64.cat
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
    \item \Variant{DIC}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show TLBI-ca aarch64.cat
  \expandafter{\MakeUppercase\TLBIcaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \TLBI{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\TLBIafter{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \ImpTTDR{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\ca{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \W{E\textsubscript{2}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show TLBI-ob aarch64.cat
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
    \item \ImpTTDR{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{4}}}.
    \item \expandafter{\MakeUppercase\sameloworderbits{E\textsubscript{4}}{E\textsubscript{5}}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{4}}{E\textsubscript{5}}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{3}}{E\textsubscript{5}}}.
    \item \ImpTTDR{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\sametranslationcontext{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \expandafter{\MakeUppercase\TTDreadorderedbefore{E\textsubscript{3}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\ext{E\textsubscript{3}}{E\textsubscript{2}}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show TTD-obs aarch64.cat
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
    \item \HU{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \W{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \W{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \HU{E\textsubscript{2}}.
    \end{itemize}
  \item \expandafter{\MakeUppercase\TLBIca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show TTD-read-ordered-before aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show Tag-obs aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show addr aarch64.cat
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
    \item \ImpTTDR{E\textsubscript{2}}.
    \item \HU{E\textsubscript{2}}.
    \item \TLBI{E\textsubscript{2}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \item \ICIVAU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show aob aarch64.cat
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
    \item \HU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show bob aarch64.cat
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
        \item \Exp{E\textsubscript{1}}.
        \item \R{E\textsubscript{1}}.
        \item \expandafter{\MakeUppercase\notthecase{\NoRet{E\textsubscript{1}}}}.
        \end{itemize}
      \item \ImpTagR{E\textsubscript{1}}.
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
    \item \ExpRQ{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\iicoorder{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpRQ{E\textsubscript{2}}.
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
  \item All of the following apply:
    \begin{itemize}
    \item \ExpWL{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\iicoorder{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item \ExpWL{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show ctrl aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show data aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show dob aarch64.cat
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
      \item \HU{E\textsubscript{2}}.
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
      \item \HU{E\textsubscript{2}}.
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
      \item \HU{E\textsubscript{2}}.
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
      \item \HU{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show dtrm aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show Exp-haz-ob aarch64.cat
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
    \item \HU{E\textsubscript{5}}.
    \end{itemize}
  \item One of the following applies:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\sca{E\textsubscript{5}}{E\textsubscript{2}}}.
    \item \expandafter{\MakeUppercase\sameEffect{E\textsubscript{5}}{E\textsubscript{2}}}.
    \end{itemize}
  \item One of the following applies:
    \begin{itemize}
    \item \ExpW{E\textsubscript{2}}.
    \item \HU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show f-ib aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show haz-ob aarch64.cat
  \expandafter{\MakeUppercase\hazobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\Exphazob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\TLBIob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\ICob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show hw-reqs aarch64.cat
  \expandafter{\MakeUppercase\hwreqsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\localhwreqs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\hazob{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show lob aarch64.cat
  \expandafter{\MakeUppercase\lobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\tcib{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\fib{E\textsubscript{1}}{E\textsubscript{2}}}.
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show local-hw-reqs aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show ob aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show obs aarch64.cat
  \expandafter{\MakeUppercase\obsemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item \Expobs{E\textsubscript{1}}{either  E\textsubscript{2}} or \sca{an Effect which}{E\textsubscript{2}}.
  \item \Tagobs{E\textsubscript{1}}{either  E\textsubscript{2}} or \sca{an Effect which}{E\textsubscript{2}}.
  \item \expandafter{\MakeUppercase\TTDobs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\Instrobs{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pick-addr-dep aarch64.cat
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
    \item \ImpTTDR{E\textsubscript{2}}.
    \item \HU{E\textsubscript{2}}.
    \item \TLBI{E\textsubscript{2}}.
    \item \DCCVAU{E\textsubscript{2}}.
    \item \ICIVAU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pick-basic-dep aarch64.cat
  \expandafter{\MakeUppercase\pickbasicdepemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\pickdtrm{E\textsubscript{1}}{E\textsubscript{2}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pick-ctrl-dep aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pick-data-dep aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pick-dep aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pick-dtrm aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pick-lob aarch64.cat
  \expandafter{\MakeUppercase\picklobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\pickdep{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \expandafter{\MakeUppercase\lob{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item \ExpW{E\textsubscript{2}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show po-scl-ob aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show pob aarch64.cat
  \expandafter{\MakeUppercase\pobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \expandafter{\MakeUppercase\pickaddrdep{E\textsubscript{1}}{E\textsubscript{2}}}.
    \item One of the following applies:
      \begin{itemize}
      \item \ExpW{E\textsubscript{2}}.
      \item \HU{E\textsubscript{2}}.
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
      \item \HU{E\textsubscript{2}}.
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
      \item \HU{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show tc-ib aarch64.cat
  \expandafter{\MakeUppercase\tcibemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \expandafter{\MakeUppercase\tcbefore{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\notthecase{\ExpR{E\textsubscript{2}}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show tr-ib aarch64.cat
  \expandafter{\MakeUppercase\tribemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpTTDR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\iicodata{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \B{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\iicoctrl{E\textsubscript{3}}{E\textsubscript{2}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpM{E\textsubscript{2}}.
    \item \MMUFAULT{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show f-ob aarch64.cat
  \expandafter{\MakeUppercase\fobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpInstrR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \expandafter{\MakeUppercase\notthecase{\ImpInstrR{E\textsubscript{2}}}}.
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show ets-ob aarch64.cat
  \expandafter{\MakeUppercase\etsobemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if one of the following applies:
  \begin{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \Variant{ETS2}.
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \TLBUncacheableFAULT{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{2}}{E\textsubscript{3}}}.
    \item \ImpTTDR{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \Variant{ETS3}.
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \MMUFAULT{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\trib{E\textsubscript{2}}{E\textsubscript{3}}}.
    \item \ImpTTDR{E\textsubscript{2}}.
    \end{itemize}
  \item All of the following apply:
    \begin{itemize}
    \item \Variant{ETS3}.
    \item \ExpM{E\textsubscript{1}}.
    \item \expandafter{\MakeUppercase\po{E\textsubscript{1}}{E\textsubscript{3}}}.
    \item \TagCheckEXCENTRY{E\textsubscript{3}}.
    \item \expandafter{\MakeUppercase\tcib{E\textsubscript{2}}{E\textsubscript{3}}}.
    \item \ImpTagR{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show tc-before aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show lrrs aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show lmrs aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show lmws aarch64.cat
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
      \item \HU{E\textsubscript{2}}.
      \end{itemize}
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show same-loc aarch64.cat
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

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show TLBuncacheable-ca aarch64.cat
  \expandafter{\MakeUppercase\TLBuncacheablecaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ImpTTDR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\trib{E\textsubscript{1}}{E\textsubscript{3}}}.
  \item \TLBUncacheableFAULT{E\textsubscript{3}}.
  \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item One of the following applies:
    \begin{itemize}
    \item \ExpW{E\textsubscript{2}}.
    \item \HU{E\textsubscript{2}}.
    \end{itemize}
  \end{itemize}

  $ miaou7 -q -set-libdir ./libdir -tex catdefinitions.tex -show HU-ca aarch64.cat
  \expandafter{\MakeUppercase\HUcaemph{an Effect E\textsubscript{1}}{an Effect E\textsubscript{2}}} if all of the following apply:
  \begin{itemize}
  \item \ExpR{E\textsubscript{1}}.
  \item \expandafter{\MakeUppercase\ca{E\textsubscript{1}}{E\textsubscript{2}}}.
  \item \HU{E\textsubscript{2}}.
  \end{itemize}
