//! Transport-neutral envelopes and the [`EnvelopeFlattener`] trait.
//!
//! An envelope pairs HTTP/Kafka headers with an optional body, for header-
//! and CloudEvents-aware flatteners that need metadata alongside (or
//! instead of) a JSON-shaped event body.

use crate::{OwnedField, QuaminaError, SegmentsTreeTracker};

/// The transport an [`Envelope`] was received over. HTTP and Kafka bind
/// CloudEvents attributes and comma-list semantics differently, so decoders
/// consult this to normalize headers before matching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Transport {
    /// HTTP request/response headers.
    Http,
    /// Kafka record headers.
    Kafka,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum EnvelopeBody {
    /// No body was sent at all (e.g. an HTTP request with no entity body).
    Absent,
    /// A Kafka record with a `null` value, distinct from an empty value.
    Tombstone,
    /// A body was sent, possibly zero-length.
    Present(Vec<u8>),
}

/// A single ordered collection of transport headers, preserving the exact
/// bytes and order they arrived in (including repeats).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Headers {
    entries: Vec<(Vec<u8>, Vec<u8>)>,
}

impl Headers {
    /// Iterate every header as `(name, value)` in transport order.
    pub fn iter(&self) -> impl Iterator<Item = (&[u8], &[u8])> {
        self.entries
            .iter()
            .map(|(name, value)| (name.as_slice(), value.as_slice()))
    }

    /// Every value for headers whose name matches `name` case-insensitively
    /// (ASCII only), in transport order.
    #[must_use]
    pub fn values(&self, name: &str) -> Vec<&[u8]> {
        self.entries
            .iter()
            .filter(|(entry_name, _)| entry_name.eq_ignore_ascii_case(name.as_bytes()))
            .map(|(_, value)| value.as_slice())
            .collect()
    }

    /// True if no header named `name` (case-insensitive) is present.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Number of header entries, counting repeats separately.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.entries.len()
    }
}

/// A transport-neutral envelope: headers plus an optional body, carrying
/// enough information to distinguish an absent body, an empty body, and a
/// Kafka tombstone (a record with a `null` value).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Envelope {
    transport: Transport,
    body: EnvelopeBody,
    headers: Headers,
}

impl Envelope {
    /// Start building an HTTP envelope with the given body bytes.
    #[must_use]
    pub fn http(body: &[u8]) -> EnvelopeBuilder {
        EnvelopeBuilder::new(Transport::Http, EnvelopeBody::Present(body.to_vec()))
    }

    /// Start building an HTTP envelope with no body sent at all.
    #[must_use]
    pub const fn http_without_body() -> EnvelopeBuilder {
        EnvelopeBuilder::new(Transport::Http, EnvelopeBody::Absent)
    }

    /// Start building a Kafka envelope with the given record value bytes.
    #[must_use]
    pub fn kafka(body: &[u8]) -> EnvelopeBuilder {
        EnvelopeBuilder::new(Transport::Kafka, EnvelopeBody::Present(body.to_vec()))
    }

    /// Start building a Kafka envelope representing a tombstone (a record
    /// whose value is `null`), distinct from an empty value.
    #[must_use]
    pub const fn kafka_tombstone() -> EnvelopeBuilder {
        EnvelopeBuilder::new(Transport::Kafka, EnvelopeBody::Tombstone)
    }

    /// The transport this envelope arrived over.
    #[must_use]
    pub const fn transport(&self) -> Transport {
        self.transport
    }

    /// This envelope's headers.
    #[must_use]
    pub const fn headers(&self) -> &Headers {
        &self.headers
    }

    /// The body bytes, if any body was sent. `None` for both an absent body
    /// and a Kafka tombstone; use [`is_tombstone`](Self::is_tombstone) to
    /// tell them apart.
    #[must_use]
    pub const fn body(&self) -> Option<&[u8]> {
        match &self.body {
            EnvelopeBody::Present(bytes) => Some(bytes.as_slice()),
            EnvelopeBody::Absent | EnvelopeBody::Tombstone => None,
        }
    }

    /// True if no body was sent at all (as opposed to an empty body).
    #[must_use]
    pub const fn is_body_absent(&self) -> bool {
        matches!(self.body, EnvelopeBody::Absent)
    }

    /// True if this is a Kafka tombstone (a record with a `null` value).
    #[must_use]
    pub const fn is_tombstone(&self) -> bool {
        matches!(self.body, EnvelopeBody::Tombstone)
    }
}

/// Builds an [`Envelope`] one header at a time.
#[derive(Debug, Clone)]
pub struct EnvelopeBuilder {
    transport: Transport,
    body: EnvelopeBody,
    headers: Vec<(Vec<u8>, Vec<u8>)>,
}

impl EnvelopeBuilder {
    const fn new(transport: Transport, body: EnvelopeBody) -> Self {
        Self {
            transport,
            body,
            headers: Vec::new(),
        }
    }

    /// Append a header with a UTF-8 name.
    #[must_use]
    pub fn header(self, name: &str, value: &[u8]) -> Self {
        self.header_bytes(name.as_bytes(), value)
    }

    /// Append a header with a raw (possibly invalid or hostile) name.
    #[must_use]
    pub fn header_bytes(mut self, name: &[u8], value: &[u8]) -> Self {
        self.headers.push((name.to_vec(), value.to_vec()));
        self
    }

    /// Append the four required CloudEvents attributes as transport-bound
    /// headers (`ce-*` over HTTP, `ce_*` over Kafka).
    #[must_use]
    pub fn cloud_event_required(self, id: &str, ty: &str, source: &str, specversion: &str) -> Self {
        let prefix = match self.transport {
            Transport::Http => "ce-",
            Transport::Kafka => "ce_",
        };
        self.header(&format!("{prefix}specversion"), specversion.as_bytes())
            .header(&format!("{prefix}id"), id.as_bytes())
            .header(&format!("{prefix}type"), ty.as_bytes())
            .header(&format!("{prefix}source"), source.as_bytes())
    }

    /// Finish building the envelope.
    ///
    /// # Errors
    /// This currently always returns `Ok`; the fallible signature is
    /// reserved for future header-shape validation performed at build time.
    pub fn build(self) -> Result<Envelope, QuaminaError> {
        Ok(Envelope {
            transport: self.transport,
            body: self.body,
            headers: Headers {
                entries: self.headers,
            },
        })
    }
}

/// Trait for flattening transport envelopes (headers plus an optional
/// body) into field lists, parallel to [`Flattener`](crate::Flattener) for
/// byte-oriented events.
pub trait EnvelopeFlattener: Send + Sync {
    /// Flatten an envelope into a list of fields.
    ///
    /// # Errors
    /// Returns an error if the envelope cannot be interpreted under this
    /// flattener's configured policies.
    fn flatten_envelope(
        &mut self,
        envelope: &Envelope,
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError>;

    /// Create an independent copy of this flattener for parallel contexts.
    fn copy(&self) -> Box<dyn EnvelopeFlattener>;
}
