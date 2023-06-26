
with child_ngrams as
(select jsonb_array_elements_text(ngrams_repo_element->'children') as term
from node_stories),
parent_ngrams as
(select ngrams_repo_element->>'root' as term
from node_stories)

(select child_ngrams.term, ngrams.terms
from child_ngrams
left join ngrams on child_ngrams.term = ngrams.terms
where ngrams.terms is null

union

select parent_ngrams.term, ngrams.terms
from parent_ngrams
left join ngrams on parent_ngrams.term = ngrams.terms
where ngrams.terms is null
and parent_ngrams.term is not null)

order by term;

