% ============================================================
% Macros: afkortingen voor formules die je vaak gebruikt
% ============================================================

% Afkorting := Formule.

iv := np\s.		% intransitief/onovergankelijk ww: 'dreams' ...
tv := iv/np.		% transitief/overgankelijk ww: 'teases' ...

% ============================================================
% Lexicon: je woordenboek
% ============================================================

% Woord :: Formule.
% Beperk je tot de volgende atomaire formules:
% s (zin), np (naam: "Alice"), n (zelfstandig naamwoord: "girl").

alice :: np.
tweedledum :: np.
tweedledee :: np.
jim :: np.
noam :: np.

queen :: n.
hatter :: n.
book :: n.
girl :: n.
mathematician :: n.
linguist :: n.
telescope :: n.

dreams :: iv.
snores :: iv.
left :: iv.

% De woorden hieronder hebben nog geen type gekregen. Bedenk een
% geschikte formule, en vul die in waar nu een vraagteken staat.
% Haal het commentaarteken weg om de woorden in het lexicon te
% stoppen. Onderaan het document vind je de voorbeeldzinnen
% die je af wil kunnen leiden. 

% the :: ?.
% a :: ?.

% mad :: ?.
% red :: ?.
% nice :: ?.
% boring :: ?.

% teases :: ?.
% irritates :: ?.
% saw :: ?.
% hit :: ?
% wrote :: ?.

% ============================================================
% Testvoorbeelden: "...." ===> Formule.
% ============================================================
% Als je oplossingen kloppen, kan je nu de zinnen
% hieronder afleiden, maar kan je geen slechte zinnen
% produceren.

"Alice dreams" ===> s.
"The Red Queen snores" ===> s.
"Alice teases the Mad Hatter" ===> s.

% Voor de persoonlijke voornaamwoorden hieronder wil je formules die
% ervoor zorgen dat je wel "she teases him" maar niet
% "him teases she" kan afleiden!

% he :: ?.
% she :: ?.
% him :: ?.
% her :: ?.

"She irritates Tweedledee" ===> s.
"Tweedledee irritates her" ===> s.

% Betrekkelijke voornaamwoorden. Bedenk eerst een goede formule
% voor de betrekkelijke bijzin: dat is een bepaling bij een naamwoord,
% net zoals een bijvoeglijk naamwoord, alleen: de bijzin volgt op het
% naamwoord, het adjectief gaat eraan vooraf. Als je een type hebt
% voor de betrekkelijke bijzin is het makkelijk om daaruit weer het
% type voor het betrekkelijk voornaamwoord af te leiden.

% that :: ?. % zoals in "the song that irritates Alice"
% that :: ?. % zoals in "the tarts that Alice stole"

% Je zal twee verschillende oplossingen voor "that" nodig hebben!

"the song that irritates the Red Queen" ===> np.
"the tarts that Alice stole" ===> np.

% ============================================================
% Ambiguiteit
% ============================================================

% De zin hieronder kan op twee manieren begrepen worden: is
% "with the telescope" een werkwoordelijke of een naamwoordelijke
% bepaling. De betekenis van "sees"/"hits" maakt telkens een van
% beide lezingen meer waarschijnlijk. Breid je lexicon uit: je
% zal voor "with" op twee verschillende typen uitkomen. Test
% dan de zinnen hieronder.

% saw :: ?.
% hits :: ?.
% man :: ?.
% telescope :: ?.
% with :: ?. % twee oplossingen!

"Alice saw the man with the telescope" ===> s.
"Alice hits the man with the telescope" ===> s.







