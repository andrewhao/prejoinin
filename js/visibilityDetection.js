/* Detect changes in side-scroller element visibility depending on window
 * scroll events. Takes a callback function that indicates to the outside
 * world whether a right arrow is necessary.
 */
export default class VisibilityDetection {
  constructor(handleNeedsRightScrollerChange) {
    this.visibilityState = {
      lastTabVisible: false,
      sideScrollerVisible: false
    };
    this.handleNeedsRightScrollerChange = handleNeedsRightScrollerChange;
  }

  registerVisibilityChangeHandler(lastTabEl, sideScrollerEl) {
    this.lastTabMonitor = VisSense(lastTabEl).monitor({
      percentagechange: (e) => {
        if (e._state.percentage >= 0 && e._state.percentage < 1) {
          this._updateVisibilityState(false);
        } else {
          this._updateVisibilityState(true);
        }
      }
    }).start();

    this.sideScrollerMonitor = VisSense(sideScrollerEl).monitor({
      percentagechange: (e) => {
        if (e._state.percentage >= 0 && e._state.percentage < 1) {
          this._updateVisibilityState(undefined, false);
        } else {
          this._updateVisibilityState(undefined, true);
        }
      }
    }).start();
  }

  _updateVisibilityState(lastTabVisible, sideScrollerVisible) {
    if (lastTabVisible !== undefined) {
      this.visibilityState.lastTabVisible = lastTabVisible;
    }
    if (sideScrollerVisible !== undefined) {
      this.visibilityState.sideScrollerVisible = sideScrollerVisible;
    }
    this._broadcastVisibilityState();
    return this.visibilityState;
  }

  _broadcastVisibilityState() {
    var needsRightScrollerArrow = !this.visibilityState.lastTabVisible && this.visibilityState.sideScrollerVisible;
    this.handleNeedsRightScrollerChange(needsRightScrollerArrow);
  };
}
