import init, { add_pattern, match_event, clear_patterns } from './pkg/quamina_playground.js';

class QuaminaPlayground {
    constructor() {
        this.isInitialized = false;
        this.rules = [];
        this.events = [];
        this.nextRuleId = 1;
        this.nextEventId = 1;
        this.debounceTimers = {};
    }

    async init() {
        await init();
        this.isInitialized = true;

        this.elements = {
            rulePattern: document.getElementById('rule-pattern'),
            clearRulesBtn: document.getElementById('clear-rules'),
            rulesList: document.getElementById('rules-list'),
            eventJson: document.getElementById('event-json'),
            clearEventsBtn: document.getElementById('clear-events'),
            eventsList: document.getElementById('events-list'),
        };

        this.setupEventListeners();
        this.loadExamples();
    }

    setupEventListeners() {
        this.elements.rulePattern.addEventListener('input', () =>
            this.debounce('newRule', () => this.handleNewRuleInput(), 250));
        this.elements.eventJson.addEventListener('input', () =>
            this.debounce('newEvent', () => this.handleNewEventInput(), 250));
        this.elements.clearRulesBtn.addEventListener('click', () => this.clearRules());
        this.elements.clearEventsBtn.addEventListener('click', () => this.clearEvents());
    }

    debounce(key, fn, delay) {
        clearTimeout(this.debounceTimers[key]);
        this.debounceTimers[key] = setTimeout(fn, delay);
    }

    handleNewRuleInput() {
        const pattern = this.elements.rulePattern.value.trim();
        if (!pattern) return;
        try {
            JSON.parse(pattern);
        } catch {
            return;
        }
        this.addRule(pattern);
        this.elements.rulePattern.value = '';
    }

    handleNewEventInput() {
        const json = this.elements.eventJson.value.trim();
        if (!json) return;
        try {
            JSON.parse(json);
        } catch {
            return;
        }
        this.addEvent(json);
        this.elements.eventJson.value = '';
    }

    rebuildMatcher() {
        clear_patterns();
        for (const rule of this.rules) {
            add_pattern(rule.id, rule.pattern);
        }
    }

    addRule(pattern) {
        const id = `rule${this.nextRuleId++}`;
        const start = performance.now();
        this.rules.push({ id, pattern, time_us: 0 });
        this.rebuildMatcher();
        const elapsed = (performance.now() - start) * 1000;
        this.rules[this.rules.length - 1].time_us = elapsed;
        this.renderRules();
        this.rerunEvents();
    }

    updateRule(index, pattern) {
        const start = performance.now();
        this.rules[index].pattern = pattern;
        this.rebuildMatcher();
        const elapsed = (performance.now() - start) * 1000;
        this.rules[index].time_us = elapsed;
        this.updateRuleTime(index);
        this.rerunEvents();
    }

    deleteRule(index) {
        this.rules.splice(index, 1);
        this.rebuildMatcher();
        this.renderRules();
        this.rerunEvents();
    }

    addEvent(json) {
        const id = `event${this.nextEventId++}`;
        const start = performance.now();
        const result = match_event(json);
        const elapsed = (performance.now() - start) * 1000;
        this.events.push({
            id, json,
            matches: result.error ? [] : result.matches,
            error: result.error || null,
            time_us: elapsed
        });
        this.renderEvents();
    }

    updateEvent(index, json) {
        const start = performance.now();
        const result = match_event(json);
        const elapsed = (performance.now() - start) * 1000;
        this.events[index].json = json;
        this.events[index].matches = result.error ? [] : result.matches;
        this.events[index].error = result.error || null;
        this.events[index].time_us = elapsed;
    }

    deleteEvent(index) {
        this.events.splice(index, 1);
        this.renderEvents();
    }

    rerunEvents() {
        this.events.forEach((event, index) => {
            const start = performance.now();
            const result = match_event(event.json);
            const elapsed = (performance.now() - start) * 1000;
            event.matches = result.error ? [] : result.matches;
            event.error = result.error || null;
            event.time_us = elapsed;
            this.updateEventDisplay(index);
        });
    }

    clearRules() {
        clear_patterns();
        this.rules = [];
        this.nextRuleId = 1;
        this.renderRules();
        this.rerunEvents();
    }

    clearEvents() {
        this.events = [];
        this.nextEventId = 1;
        this.renderEvents();
    }

    renderRules() {
        if (this.rules.length === 0) {
            this.elements.rulesList.innerHTML = '<div class="info">Type a pattern below to add a rule</div>';
            return;
        }

        this.elements.rulesList.innerHTML = this.rules.map((rule, index) => `
            <div class="item rule-item" role="listitem" data-index="${index}">
                <div class="item-header">
                    <span class="item-id">${this.escapeHtml(rule.id)}</span>
                    <div class="item-actions">
                        <span class="item-time" aria-label="Build time">${this.formatTime(rule.time_us)}</span>
                        <button class="delete-btn" data-index="${index}" type="button" aria-label="Delete ${rule.id}" title="Delete">&times;</button>
                    </div>
                </div>
                <textarea
                    class="inline-edit"
                    data-index="${index}"
                    spellcheck="false"
                    aria-label="Edit ${rule.id} pattern"
                >${this.escapeHtml(rule.pattern)}</textarea>
            </div>
        `).join('');

        this.attachRuleListeners();
    }

    attachRuleListeners() {
        this.elements.rulesList.querySelectorAll('.inline-edit').forEach(textarea => {
            textarea.addEventListener('input', () => {
                const index = parseInt(textarea.dataset.index);
                this.debounce(`rule-${index}`, () => {
                    const pattern = textarea.value.trim();
                    try {
                        JSON.parse(pattern);
                        this.updateRule(index, pattern);
                        textarea.classList.remove('invalid');
                    } catch {
                        textarea.classList.add('invalid');
                    }
                }, 300);
            });
        });

        this.elements.rulesList.querySelectorAll('.delete-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                this.deleteRule(parseInt(btn.dataset.index));
            });
        });
    }

    updateRuleTime(index) {
        const timeEl = this.elements.rulesList.querySelector(`[data-index="${index}"] .item-time`);
        if (timeEl) timeEl.textContent = this.formatTime(this.rules[index].time_us);
    }

    renderEvents() {
        if (this.events.length === 0) {
            this.elements.eventsList.innerHTML = '<div class="info">Type an event below to test</div>';
            return;
        }

        this.elements.eventsList.innerHTML = this.events.map((event, index) => `
            <div class="item event-item" role="listitem" data-index="${index}">
                <div class="item-header">
                    <span class="item-id">${this.escapeHtml(event.id)}</span>
                    <div class="item-actions">
                        <span class="item-time" aria-label="Match time">${this.formatTime(event.time_us)}</span>
                        <button class="delete-btn" data-index="${index}" type="button" aria-label="Delete ${event.id}" title="Delete">&times;</button>
                    </div>
                </div>
                <textarea
                    class="inline-edit"
                    data-index="${index}"
                    spellcheck="false"
                    aria-label="Edit ${event.id} JSON"
                >${this.escapeHtml(event.json)}</textarea>
                <div class="item-matches" aria-live="polite">${this.renderMatches(event)}</div>
            </div>
        `).join('');

        this.attachEventListeners();
    }

    attachEventListeners() {
        this.elements.eventsList.querySelectorAll('.inline-edit').forEach(textarea => {
            textarea.addEventListener('input', () => {
                const index = parseInt(textarea.dataset.index);
                this.debounce(`event-${index}`, () => {
                    const json = textarea.value.trim();
                    try {
                        JSON.parse(json);
                        this.updateEvent(index, json);
                        textarea.classList.remove('invalid');
                        this.updateEventDisplay(index);
                    } catch {
                        textarea.classList.add('invalid');
                    }
                }, 300);
            });
        });

        this.elements.eventsList.querySelectorAll('.delete-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                this.deleteEvent(parseInt(btn.dataset.index));
            });
        });
    }

    renderMatches(event) {
        if (event.error) return `<span class="error-inline">${this.escapeHtml(event.error)}</span>`;
        if (event.matches.length === 0) return '<span class="no-match">No matches</span>';
        return event.matches.map(m => `<span class="match-tag">${this.escapeHtml(m)}</span>`).join('');
    }

    updateEventDisplay(index) {
        const event = this.events[index];
        const item = this.elements.eventsList.querySelector(`.event-item[data-index="${index}"]`);
        if (!item) return;
        const matchesEl = item.querySelector('.item-matches');
        if (matchesEl) matchesEl.innerHTML = this.renderMatches(event);
        const timeEl = item.querySelector('.item-time');
        if (timeEl) timeEl.textContent = this.formatTime(event.time_us);
    }

    formatTime(us) {
        if (us < 1) return `${(us * 1000).toFixed(0)}ns`;
        if (us < 1000) return `${us.toFixed(1)}µs`;
        return `${(us / 1000).toFixed(2)}ms`;
    }

    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    loadExamples() {
        // Demo rules showcasing different features:
        // Note: quamina uses I-Regexp (RFC 9485) where ~ is the escape char, not \
        // So ~. matches a literal dot, ~~ matches a tilde
        const exampleRules = [
            '{"status": ["active"], "level": [{"numeric": ["=", 5]}]}',
            '{"email": [{"regexp": "[a-z]+@[a-z]+~.[a-z]+"}]}',
            '{"email": [{"suffix": ".com"}]}'
        ];

        exampleRules.forEach(pattern => {
            const id = `rule${this.nextRuleId++}`;
            this.rules.push({ id, pattern, time_us: 0 });
        });

        const start = performance.now();
        this.rebuildMatcher();
        const elapsed = (performance.now() - start) * 1000 / this.rules.length;
        this.rules.forEach(r => r.time_us = elapsed);
        this.renderRules();

        // Demo events that show different match scenarios
        const exampleEvents = [
            '{"status": "active", "level": 5, "email": "user@example.com"}',
            '{"status": "pending", "level": 7, "email": "admin@corp.com"}'
        ];

        exampleEvents.forEach(json => {
            const id = `event${this.nextEventId++}`;
            const start = performance.now();
            const result = match_event(json);
            const elapsed = (performance.now() - start) * 1000;
            this.events.push({
                id, json,
                matches: result.error ? [] : result.matches,
                error: result.error || null,
                time_us: elapsed
            });
        });
        this.renderEvents();
    }
}

const playground = new QuaminaPlayground();
playground.init()
    .then(() => console.log('quamina-rs playground ready'))
    .catch(err => {
        document.body.innerHTML = `
            <div style="padding:2rem;text-align:center;color:#c0392b;">
                <h1>Failed to load playground</h1>
                <p>${err.message}</p>
                <p>Run <code>./build.sh</code> first.</p>
            </div>`;
    });
